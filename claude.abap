REPORT zpriority.

"! Exception class for Priority related errors
CLASS zcx_priority_exception DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_priority_error,
        no_bc_found TYPE sotr_conc VALUE '000001',
        invalid_bc  TYPE sotr_conc VALUE '000002',
      END OF gc_priority_error.

    METHODS constructor
      IMPORTING
        textid   LIKE if_t100_message=>t100key OPTIONAL
        previous LIKE previous OPTIONAL.
ENDCLASS.

CLASS zcx_priority_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
  ENDMETHOD.
ENDCLASS.

"! Main Priority handling class
CLASS priority DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_ordenation,
        index        TYPE edi_clustc,
        inc          TYPE zca_tquermessebc-inc,
        descricao_oc TYPE zca_tquermessebc-descricao_oc,
        label_oc     TYPE zca_tquermessebc-label_oc,
        bc           TYPE zca_tquermessebc-bc,
      END OF ty_ordenation,
      tt_ordenation TYPE STANDARD TABLE OF ty_ordenation WITH EMPTY KEY.

    "! Gets the responsible BC user
    "! @returns User ID of responsible BC
    "! @raising zcx_priority_exception if no BC is found
    METHODS get_responsable
      RETURNING
        VALUE(rv_result) TYPE zca_tquermessebc-bc
      RAISING
        zcx_priority_exception.

    "! Displays BC list for given BC ID
    "! @parameter iv_bc BC ID to display list for
    "! @raising zcx_priority_exception if BC ID is invalid
    METHODS display_bc_list
      IMPORTING
        iv_bc TYPE zca_tquermessebc-bc
      RAISING
        zcx_priority_exception.

    "! Gets BC list for given BC ID
    "! @parameter iv_bc BC ID to get list for
    "! @returns Table of BC entries
    "! @raising zcx_priority_exception if BC ID is invalid
    METHODS get_bc_list
      IMPORTING
        iv_bc          TYPE zca_tquermessebc-bc
      RETURNING
        VALUE(rt_list) TYPE tt_ordenation
      RAISING
        zcx_priority_exception.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_bc,
        bc        TYPE zca_tquermessebc-bc,
        name_text TYPE adrp-name_text,
      END OF ty_bc,
      tt_bc TYPE SORTED TABLE OF ty_bc WITH UNIQUE KEY bc.

    CONSTANTS:
      BEGIN OF gc_status,
        atribuido TYPE zca_tquermessebc-estat VALUE 'E0009',
      END OF gc_status.

    DATA:
      mo_log TYPE REF TO zcl_logger.  " Logger instance

    METHODS:
      get_all_bc_working
        RETURNING
          VALUE(rt_result) TYPE tt_bc
        RAISING
          zcx_priority_exception,

      get_bc_working
        IMPORTING
          it_list        TYPE tt_bc
        RETURNING
          VALUE(rv_result) TYPE zca_tquermessebc-bc
        RAISING
          zcx_priority_exception,

      get_list_from_bc
        IMPORTING
          iv_bc          TYPE zca_tquermessebc-bc
        RETURNING
          VALUE(rt_result) TYPE tt_ordenation
        RAISING
          zcx_priority_exception,

      display_order_list
        IMPORTING
          it_data TYPE tt_ordenation.

ENDCLASS.

CLASS priority IMPLEMENTATION.

  METHOD get_responsable.
    TRY.
        DATA(lt_bc_list) = get_all_bc_working( ).

        IF lt_bc_list IS INITIAL.
          RAISE EXCEPTION TYPE zcx_priority_exception
            EXPORTING
              textid = zcx_priority_exception=>gc_priority_error-no_bc_found.
        ENDIF.

        rv_result = get_bc_working( lt_bc_list ).

      CATCH cx_root INTO DATA(lo_error).
        mo_log->add_exception( lo_error ).
        RAISE EXCEPTION TYPE zcx_priority_exception
          EXPORTING
            previous = lo_error.
    ENDTRY.
  ENDMETHOD.

  METHOD display_bc_list.
    TRY.
        IF iv_bc IS INITIAL.
          RAISE EXCEPTION TYPE zcx_priority_exception
            EXPORTING
              textid = zcx_priority_exception=>gc_priority_error-invalid_bc.
        ENDIF.

        DATA(lt_inc_list) = get_list_from_bc( iv_bc ).
        display_order_list( lt_inc_list ).

      CATCH cx_root INTO DATA(lo_error).
        mo_log->add_exception( lo_error ).
        RAISE EXCEPTION TYPE zcx_priority_exception
          EXPORTING
            previous = lo_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_all_bc_working.
    SELECT DISTINCT bc
      FROM zca_tquermessebc
      WHERE estat = @gc_status-atribuido
      ORDER BY bc
      INTO TABLE @DATA(lt_bc).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Get user names in one select
    SELECT u~bname AS bc,
           a~name_text
      FROM usr21 AS u
      INNER JOIN adrp AS a ON u~persnumber = a~persnumber
      FOR ALL ENTRIES IN @lt_bc
      WHERE u~bname = @lt_bc-bc
      INTO TABLE @rt_result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_priority_exception
        EXPORTING
          textid = zcx_priority_exception=>gc_priority_error-no_bc_found.
    ENDIF.
  ENDMETHOD.

  METHOD get_bc_working.
    CHECK it_list IS NOT INITIAL.

    DATA: lv_selected_bc TYPE ty_bc.

    " Use F4 help to let user select BC
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'BC'
        window_title    = 'Select BC'
        value_org      = 'S'
      TABLES
        value_tab      = it_list
      CHANGING
        value         = lv_selected_bc
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS         = 3.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_priority_exception
        EXPORTING
          textid = zcx_priority_exception=>gc_priority_error-no_bc_found.
    ENDIF.

    rv_result = lv_selected_bc-bc.
  ENDMETHOD.

  METHOD get_list_from_bc.
    CHECK iv_bc IS NOT INITIAL.

    " Get all relevant fields in one select
    SELECT seq_nr AS index,
           inc,
           descricao_oc,
           label_oc,
           bc
      FROM zca_tquermessebc
      WHERE bc    = @iv_bc
        AND estat = @gc_status-atribuido
      ORDER BY seq_nr
      INTO CORRESPONDING FIELDS OF TABLE @rt_result.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_priority_exception
        EXPORTING
          textid = zcx_priority_exception=>gc_priority_error-invalid_bc.
    ENDIF.
  ENDMETHOD.

  METHOD display_order_list.
    " Use ALV Grid for display
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = it_data ).

        " Enable all standard functions
        lo_alv->get_functions( )->set_all( abap_true ).

        " Set column optimized
        lo_alv->get_columns( )->set_optimize( abap_true ).

        " Display the ALV Grid
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lo_error).
        mo_log->add_exception( lo_error ).
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_bc_list.
    rt_list = get_list_from_bc( iv_bc ).
  ENDMETHOD.

ENDCLASS.

"! Drag and Drop handler class
CLASS lcl_drag_drop_handler DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          io_grid TYPE REF TO cl_gui_alv_grid,

      on_drag FOR EVENT ondrag OF cl_gui_alv_grid
        IMPORTING
          es_row_no
          e_column
          e_dragdropobj,

      on_drop FOR EVENT ondrop OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          e_dragdropobj,

      on_drop_complete FOR EVENT ondropcomplete OF cl_gui_alv_grid
        IMPORTING
          e_row
          e_column
          e_dragdropobj.

  PRIVATE SECTION.
    DATA:
      mo_grid     TYPE REF TO cl_gui_alv_grid,
      mt_data     TYPE priority=>tt_ordenation,
      mo_dragdrop TYPE REF TO cl_dragdrop.

ENDCLASS.

CLASS lcl_drag_drop_handler IMPLEMENTATION.
  METHOD constructor.
    mo_grid = io_grid.
    
    " Initialize drag & drop
    mo_dragdrop = NEW #( ).
    mo_dragdrop->add(
        flavor     = 'LINE'
        dragsrc    = abap_true
        droptarget = abap_true
        effect     = cl_dragdrop=>move ).

    " Set layout for drag & drop
    DATA(ls_layout) = VALUE lvc_s_layo(
      s_dragdrop = VALUE #( row_ddid = mo_dragdrop->get_handle( ) ) ).
    
    mo_grid->set_frontend_layout( ls_layout ).
  ENDMETHOD.

  METHOD on_drag.
    " Store dragged line
    READ TABLE mt_data ASSIGNING FIELD-SYMBOL(<ls_data>)
         INDEX es_row_no-row_id.
    CHECK sy-subrc = 0.

    e_dragdropobj->object = NEW lcl_drag_drop_object(
        line  = <ls_data>
        index = es_row_no-row_id ).
  ENDMETHOD.

  METHOD on_drop.
    DATA lo_drag_object TYPE REF TO lcl_drag_drop_object.
    
    " Get dragged object
    lo_drag_object ?= e_dragdropobj->object.
    CHECK lo_drag_object IS BOUND.

    " Update table
    DELETE mt_data INDEX lo_drag_object->index.
    INSERT lo_drag_object->line INTO mt_data INDEX e_row-index.
  ENDMETHOD.

  METHOD on_drop_complete.
    mo_grid->refresh_table_display( ).
  ENDMETHOD.

ENDCLASS.

"! Material handling class
CLASS lcl_material DEFINITION
  CREATE PUBLIC
  FINAL.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_data,
        material  TYPE bapimathead-material,
        matl_desc TYPE bapi_makt-matl_desc,
      END OF ty_data.

    METHODS:
      constructor
        IMPORTING
          iv_material TYPE matnr
          iv_desc     TYPE makt-maktx
        RAISING
          cx_bapi_error,

      change
        RETURNING
          VALUE(rs_return) TYPE bapiret2
        RAISING
          cx_bapi_error.

  PRIVATE SECTION.
    DATA:
      ms_data        TYPE ty_data,
      ms_header      TYPE bapimathead,
      mt_description TYPE tt_bapi_makt,
      mo_log        TYPE REF TO zcl_logger.

    METHODS:
      validate_data
        RAISING
          cx_bapi_error,

      fill_data,

      call_bapi
        RETURNING
          VALUE(rs_return) TYPE bapiret2
        RAISING
          cx_bapi_error.
ENDCLASS.

CLASS lcl_material IMPLEMENTATION.

  METHOD constructor.
    ms_data = VALUE #(
      material  = iv_material
      matl_desc = iv_desc ).
    
    mo_log = NEW #( object = 'MATERIAL' subobject = 'CHANGE' ).
  ENDMETHOD.

  METHOD change.
    validate_data( ).
    fill_data( ).
    rs_return = call_bapi( ).
  ENDMETHOD.

  METHOD validate_data.
    IF ms_data-material IS INITIAL.
      RAISE EXCEPTION TYPE cx_bapi_error
        EXPORTING
          textid = cx_bapi_error=>material_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD fill_data.
    " Fill header data
    ms_header = VALUE #(
      material = |{ ms_data-material ALPHA = OUT }| ).

    " Fill description
    mt_description = VALUE #( (
      langu     = sy-langu
      matl_desc = ms_data-matl_desc ) ).
  ENDMETHOD.

  METHOD call_bapi.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata            = ms_header
      IMPORTING
        return             = rs_return
      TABLES
        materialdescription = mt_description.

    IF rs_return-type = 'E' OR rs_return-type = 'A'.
      mo_log->add_msg( rs_return ).
      RAISE EXCEPTION TYPE cx_bapi_error.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
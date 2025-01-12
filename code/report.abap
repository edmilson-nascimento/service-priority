REPORT priory.

TYPE-POOLS: abap, cntb.

**
*DATA: carrid TYPE alv_t_t2-carrid,
*      connid TYPE alv_t_t2-connid.
*
** selection-screen
*SELECTION-SCREEN BEGIN OF BLOCK tab WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS move_row RADIOBUTTON GROUP tab1.
*SELECTION-SCREEN COMMENT 5(79) TEXT-002.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS chng_row RADIOBUTTON GROUP tab1.
*SELECTION-SCREEN COMMENT 5(79) TEXT-003.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS chng_col RADIOBUTTON GROUP tab1.
*SELECTION-SCREEN COMMENT 5(79) TEXT-004.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS copy_cel RADIOBUTTON GROUP tab1.
*SELECTION-SCREEN COMMENT 5(79) TEXT-005.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS data_cel RADIOBUTTON GROUP tab1.
*SELECTION-SCREEN COMMENT 5(79) TEXT-006.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK tab.
*
** parameters for the selection screen
*PARAMETERS: pa_car LIKE carrid DEFAULT 'LH',
*            pa_con LIKE connid DEFAULT '402'.
*SELECTION-SCREEN SKIP.
*PARAMETERS: pa_rows TYPE i DEFAULT '200'.
*PARAMETERS: pa_del TYPE char1 DEFAULT 'X'.


CLASS lcl_order_list DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_ordenation,
        inc          TYPE zca_tquermessebc-inc,
        descricao_oc TYPE zca_tquermessebc-descricao_oc,
        label_oc     TYPE zca_tquermessebc-label_oc,
        bc           TYPE zca_tquermessebc-bc,
      END OF ty_ordenation,
      tab_ordenation TYPE STANDARD TABLE OF ty_ordenation WITH DEFAULT KEY.

    METHODS get_responsable
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc.

    METHODS display_bc_list
      IMPORTING im_bc TYPE zca_tquermessebc-bc.

    METHODS get_bc_list
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE lcl_order_list=>tab_ordenation.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_bc,
        bc        TYPE zca_tquermessebc-bc,
        name_text TYPE adrp-name_text,
      END OF ty_bc,
      tab_bc TYPE STANDARD TABLE OF ty_bc WITH DEFAULT KEY.

    CONSTANTS atribuido TYPE zca_tquermessebc-estat VALUE 'E0009'.

    METHODS get_all_bc_working
      RETURNING VALUE(result) TYPE tab_bc.

    METHODS get_bc_working
      IMPORTING im_list       TYPE tab_bc
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc.

    METHODS get_list_from_bc
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE tab_ordenation.

    METHODS display_order_list
      IMPORTING im_data TYPE tab_ordenation.

ENDCLASS.

CLASS lcl_order_list IMPLEMENTATION.

  METHOD get_responsable.

    result = me->get_bc_working( me->get_all_bc_working( ) ).

  ENDMETHOD.

  METHOD display_bc_list.

    IF im_bc IS INITIAL.
      " TODO exception
      RETURN.
    ENDIF.

    DATA(inc_list) = me->get_list_from_bc( im_bc ).
    " TODO exception

    me->display_order_list( inc_list ).

  ENDMETHOD.

  METHOD get_all_bc_working.

    SELECT FROM zca_tquermessebc
      FIELDS seq_nr, bc, estat
      WHERE estat = @atribuido
      INTO TABLE @DATA(lt_data).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_bc) = VALUE tab_bc( FOR GROUPS user OF l IN lt_data
                                GROUP BY l-bc ASCENDING
                                ( bc = user ) ).

    SELECT
      FROM usr21 AS u
             INNER JOIN
               adrp AS a ON u~persnumber = a~persnumber
      FIELDS u~bname,
             a~name_text
      FOR ALL ENTRIES IN @lt_bc
      WHERE u~bname = @lt_bc-bc
      INTO TABLE @DATA(lt_user_name).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = VALUE #( FOR r IN lt_bc
                      ( bc        = r-bc
                        name_text = VALUE #( lt_user_name[ bname = r-bc ]-name_text OPTIONAL ) ) ).

  ENDMETHOD.

  METHOD get_bc_working.

    DATA:
      select_value TYPE ty_bc,
      fields       TYPE STANDARD TABLE OF help_value,
      valuetab     TYPE tab_bc.

    IF lines( im_list ) = 0.
      RETURN.
    ENDIF.

    fields = VALUE #( ( tabname    = 'ZCA_TQUERMESSEBC'
                        fieldname  = 'BC'
                        selectflag = 'X' ) ).

    valuetab = VALUE #( FOR l IN im_list
                        ( bc        = l-bc
                          name_text = l-name_text ) ).

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
      IMPORTING  select_value              = select_value                " selected value
      TABLES     fields                    = fields                 " internal table for transfer of the
                 valuetab                  = valuetab                 " internal table for transfer of the
      EXCEPTIONS field_not_in_ddic         = 1                " Table field not listed in the Dict
                 more_then_one_selectfield = 2                " During selection, only transfer of
                 no_selectfield            = 3                " No field selected for transfer
                 OTHERS                    = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = select_value-bc.

  ENDMETHOD.

  METHOD get_list_from_bc.

    IF im_bc IS INITIAL.
      RETURN.
    ENDIF.

    SELECT FROM zca_tquermessebc
      FIELDS seq_nr, bc, inc, descricao_oc, label_oc, estat
      WHERE bc    = @im_bc
        AND estat = @atribuido
      INTO TABLE @DATA(lt_data).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = VALUE #( FOR l IN lt_data
                      ( inc          = l-inc
                        descricao_oc = l-descricao_oc
                        label_oc     = l-label_oc
                        bc           = l-bc ) ).
  ENDMETHOD.

  METHOD display_order_list.

    " Temporario
    cl_demo_output=>display( data = im_data ).

  ENDMETHOD.

  METHOD get_bc_list.

    result = me->get_list_from_bc( im_bc ).

  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION DEFERRED.


DATA: application  TYPE REF TO application,
      container    TYPE REF TO cl_gui_docking_container,
      grid         TYPE REF TO cl_gui_alv_grid,
      ok_code      TYPE sy-ucomm,
      save_ok_code TYPE ok_code,
      gs_layout    TYPE lvc_s_layo,
      gt_list      TYPE lcl_order_list=>tab_ordenation.


*&---------------------------------------------------------------------*
*&       Class DRAG_DROP_OBJECT
*&---------------------------------------------------------------------*
CLASS drag_drop_object DEFINITION.

  PUBLIC SECTION.

    DATA:
      line_help  TYPE lcl_order_list=>ty_ordenation,
      index_help TYPE i.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class APPLICATION
*&---------------------------------------------------------------------*
CLASS application DEFINITION.

  PUBLIC SECTION.
    " methods for D&D handling
    METHODS handle_grid_drag FOR EVENT ondrag OF cl_gui_alv_grid
      IMPORTING es_row_no e_column e_dragdropobj.

    METHODS handle_grid_drop FOR EVENT ondrop OF cl_gui_alv_grid
      IMPORTING e_row e_column e_dragdropobj.

    METHODS handle_grid_drop_complete FOR EVENT ondropcomplete OF
                 cl_gui_alv_grid
      IMPORTING e_row e_column e_dragdropobj.

    CLASS-METHODS create_controls.

  PRIVATE SECTION.
    CLASS-METHODS build_and_assign_handler.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class (Implementation)  application
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS application IMPLEMENTATION.

  METHOD handle_grid_drag.

    DATA: data_object TYPE REF TO drag_drop_object,
          " TODO: variable is assigned but never used (ABAP cleaner)
          help_row    TYPE lcl_order_list=>ty_ordenation.

    READ TABLE gt_list INTO help_row INDEX es_row_no-row_id.

    data_object = NEW #( ).

    data_object->index_help = es_row_no-row_id.

    READ TABLE gt_list INTO data_object->line_help INDEX
    es_row_no-row_id.

    e_dragdropobj->object = data_object.

  ENDMETHOD.


  METHOD handle_grid_drop.

    DATA data_object TYPE REF TO drag_drop_object.

    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.

      data_object ?= e_dragdropobj->object.

      DELETE gt_list INDEX data_object->index_help.
      INSERT data_object->line_help INTO gt_list INDEX e_row-index.

    ENDCATCH.

  ENDMETHOD.


  METHOD handle_grid_drop_complete.

*    IF e_dragdropobj->effect = cl_dragdrop=>copy.
*      IF chng_col = 'X'.
*        CALL METHOD grid->get_frontend_fieldcatalog
*          IMPORTING
*            et_fieldcatalog = fieldcat.
** if you want to change the positions of the columns in the table
**by d&d'ing one cell of the first column to another cell of the second
**column
*        LOOP AT fieldcat[] INTO ls_fieldcat WHERE fieldname = 'PRICE'.
*          IF ls_fieldcat-col_pos = '4'.
*            ls_fieldcat-col_pos = '10'.
*            MODIFY fieldcat[] FROM ls_fieldcat.
*          ENDIF.
*          IF ls_fieldcat-col_pos = '9'.
*            ls_fieldcat-col_pos = '4'.
*            MODIFY fieldcat[] FROM ls_fieldcat.
*          ENDIF.
*        ENDLOOP.
*        CLEAR ls_fieldcat.
*        LOOP AT fieldcat[] INTO ls_fieldcat WHERE fieldname =
*        'PAYMENTSUM'.
*          IF ls_fieldcat-col_pos = '4'.
*            ls_fieldcat-col_pos = '10'.
*            MODIFY fieldcat[] FROM ls_fieldcat.
*          ENDIF.
*          IF ls_fieldcat-col_pos = '9'.
*            ls_fieldcat-col_pos = '4'.
*            MODIFY fieldcat[] FROM ls_fieldcat.
*          ENDIF.
*        ENDLOOP.
** set the fieldcatalog to make the changes at the backend
*        CALL METHOD grid->set_frontend_fieldcatalog
*          EXPORTING
*            it_fieldcatalog = fieldcat[].
*
*      ENDIF.
*    ENDIF.
*
*    IF data_cel = ' '.
* refresh the table display to make the changes visible at the frontend
    grid->refresh_table_display( ).
*    ENDIF.
*
*    IF sy-subrc <> 0.
*      CALL METHOD e_dragdropobj->abort.
*    ENDIF.

  ENDMETHOD.


  METHOD create_controls.


    " creation of the ALV Grid Control via a docking container
    container = NEW #( dynnr     = '100'
                       extension = 312
                       side      = cl_gui_docking_container=>dock_at_top ).

    grid = NEW #( i_parent = container ).

    application = NEW #( ).

    " registrate the methods
    SET HANDLER application->handle_grid_drag FOR grid.
    SET HANDLER application->handle_grid_drop FOR grid.
    SET HANDLER application->handle_grid_drop_complete FOR grid.

    build_and_assign_handler( ).

    DATA(object) = NEW lcl_order_list( ).

    DATA(bc) = object->get_responsable( ).
    IF bc IS INITIAL.
      RETURN.
    ENDIF.
    gt_list = object->get_bc_list( bc ).

*  gs_layout =

    DATA(lt_fieldcatalog) = VALUE lvc_t_fcat( tabname = 'ZCA_TQUERMESSEBC'
                                              ( row_pos   = 1
                                                fieldname = 'INC' )
                                              ( row_pos   = 2
                                                fieldname = 'DESCRICAO_OC' )
                                              ( row_pos   = 3
                                                fieldname = 'LABEL_OC' )
                                              ( row_pos   = 4
                                                fieldname = 'BC' ) ).

    grid->set_table_for_first_display( EXPORTING is_layout       = gs_layout
                                       CHANGING  it_fieldcatalog = lt_fieldcatalog
                                                 it_outtab       = gt_list ).
  ENDMETHOD.


  METHOD build_and_assign_handler.

    DATA handle_grid TYPE i.

    DATA(grid_behaviour) = NEW cl_dragdrop( ).
    IF grid_behaviour IS NOT BOUND.
      RETURN.
    ENDIF.

    grid_behaviour->add( flavor         = 'LINE'
                         dragsrc        = 'X'
                         droptarget     = 'X'
                         effect_in_ctrl = cl_dragdrop=>move ).

    grid_behaviour->get_handle( IMPORTING handle = handle_grid ).
    gs_layout-zebra   = abap_on.
*    gs_layout-col_opt = abap_on.
    gs_layout-s_dragdrop-row_ddid = handle_grid.

  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'STATUS_0100'.

  IF grid IS BOUND.
    RETURN.
  ENDIF.

  application=>create_controls( ).

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok_code = ok_code.
  CLEAR ok_code.

  " check the functions' code after your input
  CASE save_ok_code.
    WHEN 'EXIT' OR 'BACK'.
      IF container IS NOT INITIAL.
        container->free( EXCEPTIONS cntl_system_error = 1
                                    cntl_error        = 2 ).
        IF sy-subrc <> 0.
          MESSAGE a000(>0).
        ENDIF.
        cl_gui_cfw=>flush( EXCEPTIONS cntl_system_error = 1
                                      cntl_error        = 2 ).
        IF sy-subrc <> 0.
          MESSAGE a000(>0).
        ENDIF.
        IF save_ok_code = 'EXIT'.
          LEAVE PROGRAM.
        ELSE.
          CALL SELECTION-SCREEN 1000.
        ENDIF.
      ENDIF.
    WHEN 'SAVE'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

  CLEAR save_ok_code.

ENDMODULE.


CLASS lcl_material DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_data,
        material  TYPE bapimathead-material,
        matl_desc TYPE bapi_makt-matl_desc,
      END OF ty_data,
      tab_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING im_material TYPE matnr
                im_desc     TYPE makt-maktx.

    METHODS change
      RETURNING VALUE(result) TYPE bapiret2.

  PRIVATE SECTION.

    DATA:
      gs_data        TYPE ty_data,
      gs_header      TYPE bapimathead,
      gt_description TYPE tt_bapi_makt.

    METHODS fill.

    METHODS bapi
      RETURNING VALUE(result) TYPE bapiret2.

ENDCLASS.

CLASS lcl_material IMPLEMENTATION.

  METHOD constructor.

    me->gs_data = VALUE #( material  = im_material
                           matl_desc = im_desc ).

  ENDMETHOD.


  METHOD change.

    me->fill( ).

    IF    me->gs_header               IS INITIAL
       OR lines( me->gt_description )  = 0.
      RETURN.
    ENDIF.

    result = me->bapi( ).

  ENDMETHOD.


  METHOD fill.

    me->gs_header      = VALUE #( material = |{ me->gs_data-material ALPHA = OUT }| ).
    me->gt_description = VALUE #( ( langu     = sy-langu
                                    matl_desc = me->gs_data-matl_desc ) ).

  ENDMETHOD.

  METHOD bapi.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING headdata            = me->gs_header
      IMPORTING return              = result
      TABLES    materialdescription = me->gt_description.

  ENDMETHOD.

ENDCLASS.


INITIALIZATION.


*  object->display_bc_list( bc ).

  " Listar os Incidentes do BC

* SET SCREEN 100.


START-OF-SELECTION.
  SET SCREEN 100.








  "
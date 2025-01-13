REPORT priory.

TYPE-POOLS: abap, cntb.

CLASS priority DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_ordenation,
        index        TYPE edi_clustc,
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
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

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

CLASS priority IMPLEMENTATION.

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
      gt_list      TYPE priority=>tab_ordenation.


*&---------------------------------------------------------------------*
*&       Class DRAG_DROP_OBJECT
*&---------------------------------------------------------------------*
CLASS drag_drop_object DEFINITION.

  PUBLIC SECTION.

    DATA:
      line_help  TYPE priority=>ty_ordenation,
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

    METHODS create_controls.

  PRIVATE SECTION.
    METHODS build_and_assign_handler.

    METHODS build_data
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class (Implementation)  application
*&---------------------------------------------------------------------*
CLASS application IMPLEMENTATION.

  METHOD handle_grid_drag.

    DATA data_object TYPE REF TO drag_drop_object.
*          help_row    TYPE priority=>ty_ordenation.

*    READ TABLE gt_list INTO help_row INDEX es_row_no-row_id.

    data_object = NEW #( ).

    data_object->index_help = es_row_no-row_id.

    READ TABLE gt_list INTO data_object->line_help INDEX
    es_row_no-row_id.

    e_dragdropobj->object = data_object.

  ENDMETHOD.


  METHOD handle_grid_drop.

    DATA data_object TYPE REF TO drag_drop_object.

*    CATCH SYSTEM-EXCEPTIONS move_cast_error = 1.

    data_object ?= e_dragdropobj->object.

    DELETE gt_list INDEX data_object->index_help.
    INSERT data_object->line_help INTO gt_list INDEX e_row-index.

*    ENDCATCH.

  ENDMETHOD.


  METHOD handle_grid_drop_complete.

    " refresh the table display to make the changes visible at the frontend
    grid->refresh_table_display( ).

  ENDMETHOD.


  METHOD create_controls.

    " creation of the ALV Grid Control via a docking container
    container = NEW #( dynnr     = '100'
                       extension = 312
                       side      = cl_gui_docking_container=>dock_at_top ).

    grid = NEW #( i_parent = container ).

    " registrate the methods
    SET HANDLER me->handle_grid_drag FOR grid.
    SET HANDLER me->handle_grid_drop FOR grid.
    SET HANDLER me->handle_grid_drop_complete FOR grid.

    me->build_and_assign_handler( ).
    gt_list = me->build_data( ).

    " TODO atualizar em um metodo e usar de forma dinamica
    DATA(lt_fieldcatalog) = VALUE lvc_t_fcat( tabname   = 'ZCA_TQUERMESSEBC'
                                              ref_table = 'ZCA_TQUERMESSEBC'
                                              ( row_pos   = 1
                                                fieldname = 'INDEX'
                                                ref_field = 'INDEX' )
                                              ( row_pos   = 2
                                                fieldname = 'INC'
                                                ref_field = 'INC' )
                                              ( row_pos   = 3
                                                fieldname = 'DESCRICAO_OC'
                                                outputlen = 50
                                                ref_field = 'DESCRICAO_OC' )
                                              ( row_pos   = 4
                                                fieldname = 'LABEL_OC'
                                                ref_field = 'LABEL_OC' )
                                              ( row_pos   = 5
                                                fieldname = 'BC'
                                                ref_field = 'BC' ) ).

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

    " gs_layout-zebra   = abap_on.
    " gs_layout-col_opt = abap_on.
    gs_layout-s_dragdrop-row_ddid = handle_grid.

  ENDMETHOD.


  METHOD build_data.

    DATA(object) = NEW priority( ).

    DATA(bc) = object->get_responsable( ).
    IF bc IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #( FOR l IN object->get_bc_list( bc )
                      INDEX INTO index
                      ( CORRESPONDING #( l ) ) ).

    result = VALUE #( FOR l IN object->get_bc_list( bc )
                          INDEX INTO index
                      LET base = VALUE priority=>ty_ordenation( index = CONV #( index ) )
                      IN  ( CORRESPONDING #( BASE ( base ) l ) ) ).

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

  application = NEW #( ).
  IF application IS NOT BOUND.
    RETURN.
  ENDIF.

  application->create_controls( ).

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
      ENDIF.
      LEAVE PROGRAM.

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


START-OF-SELECTION.
  SET SCREEN 100.








  "
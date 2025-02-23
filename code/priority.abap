REPORT priory.

TYPE-POOLS: abap, cntb.

CLASS priority DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_ordenation  TYPE zca_s_quermes_priority,
      tab_ordenation TYPE zca_t_quermes_priority,

      BEGIN OF ty_output,
        priority        TYPE zca_s_quermes_priority-priority,
        inc             TYPE zca_s_quermes_priority-inc,
        descricao_oc    TYPE zca_s_quermes_priority-descricao_oc,
        bc_name         TYPE zca_s_quermes_priority-bc_name,
        functional_name TYPE zca_s_quermes_priority-functional_name,
        erdat           TYPE zca_s_quermes_priority-erdat,
        erzet           TYPE zca_s_quermes_priority-erzet,
        ernam           TYPE zca_s_quermes_priority-ernam,
      END OF ty_output,
      tab_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

    METHODS get_responsable
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc.

    METHODS display_bc_list
      IMPORTING im_bc TYPE zca_tquermessebc-bc.

    METHODS get_bc_list
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Realiza a persistencia dos dados</p>
    METHODS save
      IMPORTING im_data    TYPE priority=>tab_ordenation
                im_data_db TYPE priority=>tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Retorna a lista de dados para todos os BCs</p>
    METHODS get_report_list
      RETURNING VALUE(result) TYPE priority=>tab_output.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_bc,
        user      TYPE zca_tquermessebc-bc,
        name_text TYPE adrp-name_text,
      END OF ty_bc,
      tab_bc        TYPE SORTED TABLE OF ty_bc WITH UNIQUE KEY user,

      tab_quermesse TYPE STANDARD TABLE OF zca_tquermessebc,
      tab_priority  TYPE STANDARD TABLE OF zca_tquermes_pri .


    CONSTANTS:
      BEGIN OF gc_status,
        atribuido TYPE zca_tquermessebc-estat VALUE 'E0009',
      END OF gc_status.

    data gt_name_list type tab_bc.

    METHODS get_all_bc_working
      RETURNING VALUE(result) TYPE tab_bc.

    METHODS get_bc_working
      IMPORTING im_list       TYPE tab_bc
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc.

    "! <p class="shorttext synchronized" lang="PT">Retorna a lista com os dados de acordo com o BC selecionado</p>
    METHODS get_list_from_bc
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Após a busca, retorna a construção dos dados</p>
    METHODS build_output_list
      IMPORTING im_quermesse       TYPE priority=>tab_quermesse
                im_priority        TYPE priority=>tab_priority
      EXPORTING VALUE(ex_report)   TYPE priority=>tab_output
                VALUE(ex_maintain) TYPE priority=>tab_ordenation.

    METHODS display_order_list
      IMPORTING im_data TYPE tab_ordenation.

    METHODS has_confirm
      RETURNING VALUE(result) TYPE sap_bool.

    METHODS save_data
      IMPORTING im_data    TYPE priority=>tab_ordenation
                im_data_db TYPE priority=>tab_ordenation.

    METHODS get_bc_name
      IMPORTING im_bc         TYPE priority=>ty_ordenation-bc
      RETURNING VALUE(result) TYPE priority=>ty_ordenation-bc_name.

    METHODS get_name_list
      IMPORTING im_list       TYPE priority=>tab_bc
      RETURNING VALUE(result) TYPE priority=>tab_bc.

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
      WHERE estat = @me->gc_status-atribuido
      INTO TABLE @DATA(lt_data).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_bc) = VALUE tab_bc( FOR GROUPS user OF l IN lt_data
                                GROUP BY l-bc ASCENDING
                                ( user = user ) ).

                                data(lt_user_name) = me->get_name_list( lt_bc ).

    result = VALUE #( FOR r IN lt_bc
                      ( user        = r-user
                        name_text = VALUE #( lt_user_name[ user = r-user ]-name_text OPTIONAL ) ) ).

  ENDMETHOD.

  METHOD get_bc_working.

    DATA:
      select_value TYPE ty_bc,
      fields       TYPE STANDARD TABLE OF help_value,
      valuetab     TYPE STANDARD TABLE OF ty_bc.

    IF lines( im_list ) = 0.
      RETURN.
    ENDIF.

    fields = VALUE #( ( tabname    = 'ZCA_TQUERMESSEBC'
                        fieldname  = 'BC'
                        selectflag = 'X' ) ).

    valuetab = VALUE #( FOR l IN im_list
                        ( user      = l-user
                          name_text = l-name_text ) ).

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
      IMPORTING
        select_value              = select_value     " selected value
      TABLES
        fields                    = fields           " internal table for transfer of the
        valuetab                  = valuetab         " internal table for transfer of the
      EXCEPTIONS
        field_not_in_ddic         = 1                " Table field not listed in the Dict
        more_then_one_selectfield = 2                " During selection, only transfer of
        no_selectfield            = 3                " No field selected for transfer
        OTHERS                    = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = select_value-user.

  ENDMETHOD.


  METHOD get_list_from_bc.

    IF im_bc IS INITIAL.
      RETURN.
    ENDIF.

    SELECT FROM zca_tquermessebc
*     FIELDS seq_nr, bc, inc, descricao_oc, label_oc, ernam, estat
      FIELDS *
      WHERE bc    = @im_bc
        AND estat = @me->gc_status-atribuido
      INTO TABLE @DATA(lt_data).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Recuperar ordem de atendimento ja salvas
    SELECT FROM zca_tquermes_pri
      FIELDS *
      FOR ALL ENTRIES IN @lt_data
      WHERE inc = @lt_data-inc
      INTO TABLE @DATA(lt_priority).
    IF sy-subrc = 0.
      " Desta forma, eu apanho o log mais recente para cada INC
      SORT lt_priority BY inc   ASCENDING
                       erdat
                       erzet DESCENDING.
    ENDIF.

    me->build_output_list( EXPORTING im_quermesse = lt_data
                                     im_priority  = lt_priority
                           IMPORTING ex_maintain  = result ).

  ENDMETHOD.


  METHOD build_output_list.

    DATA: ls_report_empty TYPE priority=>ty_output,
          lv_bc           TYPE priority=>ty_ordenation-bc.

  clear: ex_report, ex_maintain.

    IF lines( im_priority ) = 0.
      " Quando ainda não ha prioridades atribuidas

      ex_maintain = VALUE #( FOR l IN im_quermesse
                                 INDEX INTO index
                             LET ordenation = VALUE ty_ordenation( im_priority[ inc = l-inc ] OPTIONAL )
                             IN
                                 ( priority        = COND #( WHEN ordenation IS NOT INITIAL
                                                             THEN ordenation-priority
                                                             ELSE index )
                                   item            = ordenation-item
                                   inc             = l-inc
                                   descricao_oc    = l-descricao_oc
                                   bc              = l-bc
                                   bc_name         = me->get_bc_name( l-bc )
                                   functional      = l-ernam
                                   functional_name = me->get_bc_name( l-ernam )
                                   erdat           = COND #( WHEN ordenation IS NOT INITIAL
                                                             THEN ordenation-erdat
                                                             ELSE sy-datum )
                                   erzet           = COND #( WHEN ordenation IS NOT INITIAL
                                                             THEN ordenation-erzet
                                                             ELSE sy-uzeit )
                                   ernam           = COND #( WHEN ordenation IS NOT INITIAL
                                                             THEN ordenation-ernam
                                                             ELSE sy-uname ) ) ).
      SORT ex_maintain ASCENDING BY bc
                                    priority.
      RETURN.
    ENDIF.

    DATA(lt_report_temp) = VALUE zca_t_quermes_priority(
                                    FOR p IN im_priority
                                    INDEX INTO index
                                LET quermesse = VALUE zca_tquermessebc( im_quermesse[ inc = p-inc ] OPTIONAL )
                                IN
                                    ( priority        = COND #( WHEN p-priority IS NOT INITIAL
                                                                THEN p-priority
                                                                ELSE index )
                                      item            = p-item
                                      inc             = p-inc
                                      descricao_oc    = quermesse-descricao_oc
                                      bc              = quermesse-bc
                                      bc_name         = me->get_bc_name( quermesse-bc )
                                      functional      = quermesse-ernam
                                      functional_name = me->get_bc_name( quermesse-ernam )
                                      erdat           = COND #( WHEN p-erdat IS NOT INITIAL
                                                                THEN p-erdat
                                                                ELSE sy-datum )
                                      erzet           = COND #( WHEN p-erzet IS NOT INITIAL
                                                                THEN p-erzet
                                                                ELSE sy-uzeit )
                                      ernam           = COND #( WHEN p-ernam IS NOT INITIAL
                                                                THEN p-ernam
                                                                ELSE sy-uname ) ) ).
    SORT lt_report_temp ASCENDING BY bc
                                priority.

    " Colocar separação de linhas entre os recursos
    " Temporario (eu acho)
    LOOP AT lt_report_temp INTO DATA(ls_report).

      IF lv_bc IS INITIAL.
        lv_bc = ls_report-bc.
      ENDIF.

      IF lv_bc <> ls_report-bc.
        ex_report = VALUE #( BASE ex_report
                             ( ls_report_empty ) ).
        lv_bc = ls_report-bc.
      ENDIF.

      ex_report = VALUE #( BASE ex_report
                           ( CORRESPONDING #( ls_report ) ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD display_order_list.

    " Temporario
    cl_demo_output=>display( data = im_data ).

  ENDMETHOD.

  METHOD get_bc_list.

    result = me->get_list_from_bc( im_bc ).

  ENDMETHOD.

  METHOD save.

    IF ( me->has_confirm( ) = abap_false ).
      RETURN.
    ENDIF.

    me->save_data( im_data    = im_data
                   im_data_db = im_data_db ).

  ENDMETHOD.


  METHOD get_report_list.

    SELECT FROM zca_tquermessebc
      FIELDS *
      WHERE estat = @me->gc_status-atribuido
      INTO TABLE @DATA(lt_quermesse).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Recuperar ordem de atendimento ja salvas
    SELECT FROM zca_tquermes_pri
      FIELDS *
      FOR ALL ENTRIES IN @lt_quermesse
      WHERE inc = @lt_quermesse-inc
      INTO TABLE @DATA(lt_priority).
    IF sy-subrc = 0.
      SORT lt_priority BY inc ASCENDING
                          erdat
                          erzet DESCENDING.
    ENDIF.

    me->build_output_list( EXPORTING im_quermesse = lt_quermesse
                                     im_priority  = lt_priority
                           IMPORTING ex_report    = result ).
  ENDMETHOD.


  METHOD has_confirm.

    DATA answer TYPE sapproved.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Salvar dados'(t01)
        text_question         = 'Desejar salvar os dados?'(q01)
        text_button_1         = 'Sim'(t02)
        icon_button_1         = 'S_OKAY'
        text_button_2         = 'Não'(t03)
        icon_button_2         = 'S_NONO'
        display_cancel_button = abap_off              " Button for displaying cancel pushbutton
      IMPORTING
        answer                = answer                 " Return values: '1', '2', 'A'
      EXCEPTIONS
        text_not_found        = 1                " Diagnosis text not found
        OTHERS                = 2.
    IF sy-subrc <> 0 .
      RETURN.
    ENDIF.

    result = COND #( WHEN answer = '1'
                           THEN abap_on
                           ELSE abap_off ).

  ENDMETHOD.


  METHOD save_data.

    TYPES tab_save TYPE STANDARD TABLE OF zca_tquermes_pri WITH DEFAULT KEY.

    IF lines( im_data ) = 0.
      RETURN.
    ENDIF.

    BREAK-POINT.

    DATA(lt_save_data) = VALUE tab_save( FOR l IN im_data
                                         ( CORRESPONDING #( l ) ) ).

    " Atualizando numero do item
    LOOP AT lt_save_data ASSIGNING FIELD-SYMBOL(<line>).

      " Se a prioridade não foi alterada, matem o mesmo Numero de item
      IF line_exists( im_data_db[ inc      = <line>-inc
                                  priority = <line>-priority ] ).
        DATA(item_db) = VALUE #( im_data_db[ inc      = <line>-inc
                                             priority = <line>-priority ]-item ).
        <line>-item = item_db.
        CONTINUE.
      ENDIF.

      " Atribuindo novo item (considerar apenas itens que tivaram prioridade alterada)
      <line>-item = CONV zca_tquermes_pri-item( 1 ).

      DO 999 TIMES.
        IF line_exists( im_data_db[ inc  = <line>-inc
                                    item = <line>-item ] ).

          <line>-item = <line>-item + 1.
          CONTINUE.
        ENDIF.
        EXIT.
      ENDDO.

    ENDLOOP.

    MODIFY zca_tquermes_pri FROM TABLE lt_save_data.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD get_bc_name.

    IF im_bc IS INITIAL.
      RETURN.
    ENDIF.

    IF line_exists( me->gt_name_list[ user = im_bc ] ).
      result = VALUE #( me->gt_name_list[ user = im_bc ]-name_text ).
      RETURN.
    ENDIF.

    me->gt_name_list = VALUE #( BASE me->gt_name_list
                                ( LINES OF me->get_name_list( VALUE #( ( user = im_bc ) ) ) ) ).
    result = VALUE #( me->gt_name_list[ user = im_bc ]-name_text OPTIONAL ).

  ENDMETHOD.


  METHOD get_name_list.

    IF lines( im_list ) = 0.
      RETURN.
    ENDIF.

    SELECT
      FROM usr21 AS u
             INNER JOIN
               adrp AS a ON u~persnumber = a~persnumber
      FIELDS u~bname,
             a~name_text
      FOR ALL ENTRIES IN @im_list
      WHERE u~bname = @im_list-user
      INTO TABLE @DATA(lt_user_name).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    sort lt_user_name by bname ASCENDING.

    result = CORRESPONDING priority=>tab_bc( lt_user_name
                             MAPPING user = bname ).

  ENDMETHOD.


ENDCLASS.

CLASS application DEFINITION DEFERRED.


DATA:
  application  TYPE REF TO application,
  container    TYPE REF TO cl_gui_docking_container,
  grid         TYPE REF TO cl_gui_alv_grid,
  ok_code      TYPE sy-ucomm,
  save_ok_code TYPE ok_code,
  gs_variant   TYPE disvariant,
  gs_layout    TYPE lvc_s_layo,
  "! saved data
  gt_db_list   TYPE priority=>tab_ordenation,
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

    TYPES tab_fcode TYPE STANDARD TABLE OF sy-ucomm WITH DEFAULT KEY.

    METHODS constructor.

    " methods for D&D handling
    METHODS handle_grid_drag FOR EVENT ondrag OF cl_gui_alv_grid
      IMPORTING es_row_no e_column e_dragdropobj.

    METHODS handle_grid_drop FOR EVENT ondrop OF cl_gui_alv_grid
      IMPORTING e_row e_column e_dragdropobj.

    METHODS handle_grid_drop_complete FOR EVENT ondropcomplete OF
                cl_gui_alv_grid
      IMPORTING e_row e_column e_dragdropobj.

    "! <p class="shorttext synchronized" lang="PT">Retorna a tabela de opções a ser eliminadas no Status-gui</p>
    METHODS get_excluded
      RETURNING VALUE(result) TYPE application=>tab_fcode.

    METHODS create_controls.

  PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF gc_option_type,
    report type char01 value 'R',
    maintain type char01 value 'M',
    end OF gc_option_type.

    data gv_operation_type type char01.

    METHODS build_and_assign_handler.

    METHODS build_data
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    METHODS get_fieldcatalog
      IMPORTING im_strutucre  TYPE tabname
      RETURNING VALUE(result) TYPE lvc_t_fcat.

    "! <p class="shorttext synchronized" lang="PT">Retorn o tipo de Operação que será feita</p>
    METHODS get_operation_type
      RETURNING VALUE(result) TYPE char01.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class (Implementation)  application
*&---------------------------------------------------------------------*
CLASS application IMPLEMENTATION.


  METHOD constructor.

    clear me->gv_operation_type.

  ENDMETHOD.

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

    " Atualziar a ordem alterada
    LOOP AT gt_list ASSIGNING FIELD-SYMBOL(<line>).

      DATA(priority) = CONV zca_tquermes_pri-priority( sy-tabix ).

      " Não faz nada se a prioridade nao tiver sido alterada
      IF line_exists( gt_list[ inc      = <line>-inc
                               priority = priority ] ).
        CONTINUE.
      ENDIF.

      " atualizar dados de processamento caso a prioridade tenha sido alterada
      <line>-priority = CONV #( sy-tabix ).

      <line>-erdat    = sy-datum.
      <line>-erzet    = sy-uzeit.
      <line>-ernam    = sy-uname.

    ENDLOOP.

    " refresh the table display to make the changes visible at the frontend
    grid->refresh_table_display( ).

  ENDMETHOD.


  METHOD get_excluded.

    me->gv_operation_type = me->get_operation_type( ).

    IF me->gv_operation_type = me->gc_option_type-report.
      result = VALUE #( ( 'SAVE' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD create_controls.

    CONSTANTS lc_structure TYPE dd04d-rollname VALUE 'ZCA_S_QUERMES_PRIORITY'.

    CASE me->gv_operation_type.

      WHEN me->gc_option_type-report.

        cl_demo_output=>write_data( NEW priority( )->get_report_list( ) ).
        
          cl_demo_output=>begin_section(
        title = 'Lista de atendimento por ordem de prioridades'
    ).
        DATA(lv_html) = cl_demo_output=>get( ).

        cl_abap_browser=>show_html( title       = 'Ordem de atendimento de BCs'
                                    html_string = lv_html
                                    container   = cl_gui_container=>default_screen ).

        " force cl_gui_container=>default_screen
        WRITE space.

      WHEN me->gc_option_type-maintain.

        " creation of the ALV Grid Control via a docking container
        container = NEW #( dynnr     = '100'
                           extension = 312
                           side      = cl_gui_docking_container=>dock_at_top ).

        DATA(lt_fieldcatalog) = me->get_fieldcatalog( lc_structure ).
        grid = NEW #( i_parent = container ).
        " registrate the methods
        SET HANDLER me->handle_grid_drag FOR grid.
        SET HANDLER me->handle_grid_drop FOR grid.
        SET HANDLER me->handle_grid_drop_complete FOR grid.

        me->build_and_assign_handler( ).
        gt_list = me->build_data( ).
        gt_db_list = gt_list.

        grid->set_table_for_first_display( EXPORTING is_layout       = gs_layout
                                           CHANGING  it_fieldcatalog = lt_fieldcatalog
                                                     it_outtab       = gt_list ).
        grid->refresh_table_display( ).

      WHEN OTHERS.
        LEAVE TO SCREEN 0.

    ENDCASE.


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

    gs_layout-zebra      = abap_on.
    gs_layout-cwidth_opt = abap_on.
    gs_layout-s_dragdrop-row_ddid = handle_grid.

  ENDMETHOD.


  METHOD build_data.

    DATA(object) = NEW priority( ).

    DATA(bc) = object->get_responsable( ).
    IF bc IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #( FOR l IN object->get_bc_list( bc )
                      ( CORRESPONDING #( l ) ) ).

  ENDMETHOD.


  METHOD get_fieldcatalog.

    DATA lt_fieldcatalog TYPE lvc_t_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
                 i_structure_name       = im_strutucre     " Structure name (structure, table, view)
      CHANGING   ct_fieldcat            = lt_fieldcatalog  " Field Catalog with Field Descriptions
      EXCEPTIONS inconsistent_interface = 1                " Call parameter combination error
                 program_error          = 2                " Program Errors
                 OTHERS                 = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN lt_fieldcatalog[ fieldname = 'ITEM' ] TO FIELD-SYMBOL(<fs_line>).
    IF <fs_line> IS ASSIGNED.
      <fs_line>-no_out = abap_on.
      UNASSIGN <fs_line>.
    ENDIF.

    ASSIGN lt_fieldcatalog[ fieldname = 'BC' ] TO <fs_line>.
    IF <fs_line> IS ASSIGNED.
      <fs_line>-no_out = abap_on.
      UNASSIGN <fs_line>.
    ENDIF.

    ASSIGN lt_fieldcatalog[ fieldname = 'FUNCTIONAL' ] TO <fs_line>.
    IF <fs_line> IS ASSIGNED.
      <fs_line>-no_out = abap_on.
      UNASSIGN <fs_line>.
    ENDIF.

    result = lt_fieldcatalog.

  ENDMETHOD.


  METHOD get_operation_type.

    DATA result_popup TYPE char01.

    CALL FUNCTION 'K_KKB_POPUP_RADIO2'
      EXPORTING  i_title   = 'Opções'(o01)
                 i_text1   = 'Report'(o02)
                 i_text2   = 'Manutenção'(o03)
                 i_default = 1
      IMPORTING  i_result  = result_popup
      EXCEPTIONS cancel    = 1
                 OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = SWITCH #( result_popup
                       WHEN 1 THEN me->gc_option_type-report
                       WHEN 2 THEN me->gc_option_type-maintain ).

  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  application = NEW #( ).
  IF application IS NOT BOUND.
    RETURN.
  ENDIF.

  data(lt_excluded) = application->get_excluded( ).
  SET PF-STATUS 'STATUS_0100' EXCLUDING lt_excluded.
  SET TITLEBAR 'STATUS_0100'.

  IF grid IS BOUND.
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
      NEW priority( )->save( im_data    = gt_list
                             im_data_db = gt_db_list ).
      LEAVE PROGRAM.

    WHEN OTHERS.

  ENDCASE.

  CLEAR save_ok_code.

ENDMODULE.


INITIALIZATION.


START-OF-SELECTION.
  SET SCREEN 100.








  "
REPORT priory.

TYPE-POOLS: abap, cntb.

CLASS lcx_exception DEFINITION
*  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    CONSTANTS:
      BEGIN OF no_bc_selected,
        msgid TYPE symsgid      VALUE 'ZCA_QUERMESSE_BC',
        msgno TYPE symsgno      VALUE '111',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_bc_selected.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL.
ENDCLASS.


CLASS lcx_exception IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS priority DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_ordenation  TYPE zca_s_quermes_priority.
    TYPES tab_ordenation TYPE zca_t_quermes_priority.

    METHODS get_responsable
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc
      RAISING   lcx_exception.

    METHODS display_bc_list
      IMPORTING im_bc TYPE zca_tquermessebc-bc.

    METHODS get_bc_list
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Realiza a persistencia dos dados</p>
    METHODS save
      IMPORTING im_data TYPE priority=>tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Retorna a lista de dados para todos os BCs</p>
    METHODS get_report_list
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_bc,
             user      TYPE zca_tquermessebc-bc,
             name_text TYPE adrp-name_text,
           END OF ty_bc,
           tab_bc TYPE SORTED TABLE OF ty_bc WITH UNIQUE KEY user.

    TYPES tab_quermesse TYPE STANDARD TABLE OF zca_tquermessebc.
    TYPES tab_priority  TYPE STANDARD TABLE OF zca_tquermes_pri.

    TYPES:
      BEGIN OF ty_user_config,
        bc    TYPE zca_tquermessebc-bc,
        color TYPE lvc_t_scol,
      END OF ty_user_config,
      tab_user_config TYPE STANDARD TABLE OF ty_user_config WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF gc_status,
        atribuido TYPE zca_tquermessebc-estat VALUE 'E0009',
      END OF gc_status.

    DATA gt_name_list TYPE tab_bc.

    METHODS get_all_bc_working
      RETURNING VALUE(result) TYPE tab_bc.

    METHODS get_bc_working
      IMPORTING im_list       TYPE tab_bc
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc
      RAISING   lcx_exception.

    "! <p class="shorttext synchronized" lang="PT">Retorna a lista com os dados de acordo com o BC selecionado</p>
    METHODS get_list_from_bc
      IMPORTING im_bc         TYPE zca_tquermessebc-bc
      RETURNING VALUE(result) TYPE tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Após a busca, retorna a construção dos dados</p>
    METHODS build_maintain_output_list
      IMPORTING im_quermesse  TYPE priority=>tab_quermesse
                im_priority   TYPE priority=>tab_priority
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    "! <p class="shorttext synchronized" lang="PT">Dados de report</p>
    METHODS build_report_output_list
      IMPORTING im_quermesse  TYPE priority=>tab_quermesse
                im_priority   TYPE priority=>tab_priority
                im_user_config type priority=>tab_user_config
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    METHODS display_order_list
      IMPORTING im_data TYPE tab_ordenation.

    METHODS has_confirm
      RETURNING VALUE(result) TYPE sap_bool.

    "! <p class="shorttext synchronized" lang="PT">Prepara e faz a persistencia dos dados</p>
    "!
    "! @parameter im_data    | Dados em processamento
    "! @parameter im_data_db | Dados já salvos
    METHODS save_data
      IMPORTING im_data    TYPE priority=>tab_ordenation
                im_data_db TYPE priority=>tab_ordenation.

    METHODS get_data_bd
      IMPORTING im_data       TYPE priority=>tab_ordenation
      RETURNING VALUE(result) TYPE priority=>tab_ordenation.

    METHODS get_bc_name
      IMPORTING im_bc         TYPE priority=>ty_ordenation-bc
      RETURNING VALUE(result) TYPE priority=>ty_ordenation-bc_name.

    METHODS get_name_list
      IMPORTING im_list       TYPE priority=>tab_bc
      RETURNING VALUE(result) TYPE priority=>tab_bc.

    METHODS get_user_config
      IMPORTING im_quermesse  TYPE priority=>tab_quermesse
      RETURNING VALUE(result) TYPE priority=>tab_user_config.

ENDCLASS.


CLASS priority IMPLEMENTATION.
  METHOD get_responsable.
    result = get_bc_working( me->get_all_bc_working( ) ).

    IF result IS INITIAL.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING textid = lcx_exception=>no_bc_selected.
    ENDIF.
  ENDMETHOD.

  METHOD display_bc_list.
    IF im_bc IS INITIAL.
      " TODO exception
      RETURN.
    ENDIF.

    DATA(inc_list) = get_list_from_bc( im_bc ).
    " TODO exception

    display_order_list( inc_list ).
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

    DATA(lt_user_name) = get_name_list( lt_bc ).

    result = VALUE #( FOR r IN lt_bc
                      ( user      = r-user
                        name_text = VALUE #( lt_user_name[ user = r-user ]-name_text OPTIONAL ) ) ).
  ENDMETHOD.

  METHOD get_bc_working.
    DATA select_value TYPE ty_bc.
    DATA fields       TYPE STANDARD TABLE OF help_value.
    DATA valuetab     TYPE STANDARD TABLE OF ty_bc.

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
      IMPORTING  select_value              = select_value     " selected value
      TABLES     fields                    = fields           " internal table for transfer of the
                 valuetab                  = valuetab         " internal table for transfer of the
      EXCEPTIONS field_not_in_ddic         = 1                " Table field not listed in the Dict
                 more_then_one_selectfield = 2                " During selection, only transfer of
                 no_selectfield            = 3                " No field selected for transfer
                 OTHERS                    = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF select_value IS INITIAL.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING textid = lcx_exception=>no_bc_selected.
    ENDIF.

    result = select_value-user.
  ENDMETHOD.

  METHOD get_list_from_bc.
    IF im_bc IS INITIAL.
      RETURN.
    ENDIF.

    SELECT FROM zca_tquermessebc
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
      SORT lt_priority BY inc ASCENDING
                          item DESCENDING.
    ENDIF.

    result =
      build_maintain_output_list( im_quermesse = lt_data
                                  im_priority  = lt_priority ).
  ENDMETHOD.

  METHOD build_maintain_output_list.
    IF lines( im_quermesse ) = 0.
      RETURN.
    ENDIF.

    LOOP AT im_quermesse INTO DATA(ls_quermesse).

      DATA(index) = sy-tabix.

      DATA(ls_ordenacao) = VALUE #( im_priority[ inc = ls_quermesse-inc ] OPTIONAL ).

      APPEND VALUE #( priority        = COND #( WHEN ls_ordenacao IS NOT INITIAL
                                                THEN ls_ordenacao-priority
                                                ELSE index )
                      item            = ls_ordenacao-item
                      inc             = ls_quermesse-inc
                      descricao_oc    = ls_quermesse-descricao_oc
                      bc              = ls_quermesse-bc
                      bc_name         = get_bc_name( ls_quermesse-bc )
                      functional      = ls_quermesse-ernam
                      functional_name = get_bc_name( ls_quermesse-ernam )
                      erdat           = COND #( WHEN ls_ordenacao IS NOT INITIAL
                                                THEN ls_ordenacao-erdat
                                                ELSE sy-datum )
                      erzet           = COND #( WHEN ls_ordenacao IS NOT INITIAL
                                                THEN ls_ordenacao-erzet
                                                ELSE sy-uzeit )
                      ernam           = COND #( WHEN ls_ordenacao IS NOT INITIAL
                                                THEN ls_ordenacao-ernam
                                                ELSE sy-uname ) ) TO result.
    ENDLOOP.

    SORT result ASCENDING BY bc
                             priority.
  ENDMETHOD.


  METHOD build_report_output_list.

    IF lines( im_priority ) = 0.
      RETURN.
    ENDIF.

    LOOP AT im_priority INTO DATA(ls_priority).

      " Adicionar apenas um item a tabela
      IF line_exists( result[ inc = ls_priority-inc ] ).
        CONTINUE.
      ENDIF.

      DATA(ls_quermesse) = VALUE #( im_quermesse[ inc = ls_priority-inc ] OPTIONAL ).

      " Atribuição de cores
      DATA(ls_color) = VALUE #( im_user_config[ bc = ls_quermesse-bc ]-color OPTIONAL ).

      APPEND VALUE #( priority        = ls_priority-priority
                      item            = ls_priority-item
                      inc             = ls_priority-inc
                      descricao_oc    = ls_quermesse-descricao_oc
                      bc              = ls_quermesse-bc
                      bc_name         = get_bc_name( ls_quermesse-bc )
                      functional      = ls_quermesse-ernam
                      functional_name = get_bc_name( ls_quermesse-ernam )
                      erdat           = ls_priority-erdat
                      erzet           = ls_priority-erzet
                      ernam           = ls_priority-ernam
                      cellcolor       = ls_color ) TO result.
    ENDLOOP.

    SORT result ASCENDING BY bc_name
                             priority.
  ENDMETHOD.

  METHOD display_order_list.
    " Temporario
    cl_demo_output=>display( data = im_data ).
  ENDMETHOD.

  METHOD get_bc_list.
    result = get_list_from_bc( im_bc ).
  ENDMETHOD.

  METHOD save.
    IF me->has_confirm( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lt_data_db) = get_data_bd( im_data ).

    save_data( im_data    = im_data
               im_data_db = lt_data_db ).
  ENDMETHOD.

  METHOD get_report_list.

    SELECT FROM zca_tquermessebc
      FIELDS *
      WHERE estat = @me->gc_status-atribuido
      ORDER BY inc
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
*      SORT lt_priority BY inc   ASCENDING
*                          erdat
*                          erzet DESCENDING.
      SORT lt_priority BY inc ASCENDING
                          item DESCENDING.
    ENDIF.

    result = build_report_output_list( im_quermesse   = lt_quermesse
                                       im_priority    = lt_priority
                                       im_user_config = me->get_user_config( lt_quermesse ) ).
  ENDMETHOD.

  METHOD has_confirm.

    DATA answer TYPE sapproved.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING  titlebar              = 'Salvar dados'(t01)
                 text_question         = 'Desejar salvar os dados?'(q01)
                 text_button_1         = 'Sim'(t02)
                 icon_button_1         = 'S_OKAY'
                 text_button_2         = 'Não'(t03)
                 icon_button_2         = 'S_NONO'
                 display_cancel_button = abap_off              " Button for displaying cancel pushbutton
      IMPORTING  answer                = answer                 " Return values: '1', '2', 'A'
      EXCEPTIONS text_not_found        = 1                " Diagnosis text not found
                 OTHERS                = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = COND #( WHEN answer = '1'
                     THEN abap_on
                     ELSE abap_off ).
  ENDMETHOD.

  METHOD save_data.

    TYPES tab_save TYPE STANDARD TABLE OF zca_tquermes_pri WITH DEFAULT KEY.
    DATA lt_save_data TYPE tab_save.

    IF lines( im_data ) = 0.
      RETURN.
    ENDIF.

    " Atualizando numero do item
    LOOP AT im_data INTO DATA(ls_item).

      " Se a prioridade não foi alterada, matem o mesmo Numero de item
      IF line_exists( im_data_db[ inc      = ls_item-inc
                                  priority = ls_item-priority ] ).
        DATA(item_db) = VALUE #( im_data_db[ inc      = ls_item-inc
                                             priority = ls_item-priority ]-item ).
        ls_item-item = item_db.
        CONTINUE.
      ENDIF.

      " Atribuindo novo item (considerar apenas itens que tivaram prioridade alterada)
      ls_item-item = CONV zca_tquermes_pri-item( 1 ).

      DO 999 TIMES.
        IF line_exists( im_data_db[ inc  = ls_item-inc
                                    item = ls_item-item ] ).

          ls_item-item = ls_item-item + 1.
          CONTINUE.
        ENDIF.

        " Verificar se ao salvar ja existe mesmo ID para incidente diferente
        IF line_exists( lt_save_data[ item = ls_item-item ] ).
          ls_item-item = ls_item-item + 1.
          CONTINUE.
        ENDIF.

        EXIT.

      ENDDO.

      " Criando dados que devem ser salvos pois foram alterados neste processamento
      lt_save_data = VALUE #( BASE lt_save_data
                              ( CORRESPONDING zca_tquermes_pri( ls_item ) ) ).
    ENDLOOP.

    IF lines( lt_save_data ) = 0.
      RETURN.
    ENDIF.

    MODIFY zca_tquermes_pri FROM TABLE lt_save_data.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.

  METHOD get_data_bd.
    IF lines( im_data ) = 0.
      RETURN.
    ENDIF.

    SELECT FROM zca_tquermes_pri
      FIELDS *
      FOR ALL ENTRIES IN @im_data
      WHERE inc = @im_data-inc
      INTO TABLE @DATA(lt_data).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = CORRESPONDING priority=>tab_ordenation( lt_data ).
  ENDMETHOD.

  METHOD get_bc_name.
    IF im_bc IS INITIAL.
      RETURN.
    ENDIF.

    IF line_exists( me->gt_name_list[ user = im_bc ] ).
      result = VALUE #( me->gt_name_list[ user = im_bc ]-name_text ).
      RETURN.
    ENDIF.

    gt_name_list = VALUE #( BASE me->gt_name_list
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

    SORT lt_user_name BY bname ASCENDING.

    result = CORRESPONDING priority=>tab_bc( lt_user_name
                             MAPPING user = bname ).
  ENDMETHOD.


  METHOD get_user_config.

    CONSTANTS:
      BEGIN OF lc_color_zero,
        col TYPE lvc_s_scol-color-col VALUE 0,
        int TYPE lvc_s_scol-color-int VALUE 1,
        inv TYPE lvc_s_scol-color-inv VALUE 0,
      END OF lc_color_zero,
      BEGIN OF lc_color_one,
        col TYPE lvc_s_scol-color-col VALUE 1,
        int TYPE lvc_s_scol-color-int VALUE 1,
        inv TYPE lvc_s_scol-color-inv VALUE 0,
      END OF lc_color_one.

    IF lines( im_quermesse ) = 0.
      RETURN .
    ENDIF .

    result = VALUE priority=>tab_user_config( FOR GROUPS user OF l IN im_quermesse
                                              INDEX INTO index
                                              GROUP BY l-bc ASCENDING
                                              WITHOUT MEMBERS
                                              ( bc    = user
                                                color = VALUE lvc_t_scol( ( color = COND #( WHEN ( index MOD 2 ) = 0
                                                                                            THEN lc_color_zero
                                                                                            ELSE lc_color_one ) ) ) ) ).

  ENDMETHOD.

ENDCLASS.

CLASS application DEFINITION DEFERRED.


DATA application  TYPE REF TO application.
DATA container    TYPE REF TO cl_gui_docking_container.
DATA grid         TYPE REF TO cl_gui_alv_grid.
DATA ok_code      TYPE sy-ucomm.
DATA save_ok_code TYPE ok_code.
DATA gs_variant   TYPE disvariant.
DATA gs_layout    TYPE lvc_s_layo.
"! saved data
DATA gt_db_list   TYPE priority=>tab_ordenation.
DATA gt_list      TYPE priority=>tab_ordenation.


*&---------------------------------------------------------------------*
*&       Class DRAG_DROP_OBJECT
*&---------------------------------------------------------------------*
CLASS drag_drop_object DEFINITION.

  PUBLIC SECTION.
    DATA line_help  TYPE priority=>ty_ordenation.
    DATA index_help TYPE i.

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
        report   TYPE char01 VALUE 'R',
        maintain TYPE char01 VALUE 'M',
      END OF gc_option_type.

    DATA gv_operation_type TYPE char01.

    METHODS build_and_assign_handler.

    METHODS build_data
      RETURNING VALUE(result) TYPE priority=>tab_ordenation
      RAISING   lcx_exception.

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
    CLEAR me->gv_operation_type.
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

    gv_operation_type = get_operation_type( ).

    IF me->gv_operation_type = gc_option_type-report.
      result = VALUE #( ( 'SAVE' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD create_controls.
    CONSTANTS lc_structure TYPE dd04d-rollname VALUE 'ZCA_S_QUERMES_PRIORITY'.

    container = NEW #( dynnr     = '100'
                       extension = 312
                       side      = cl_gui_docking_container=>dock_at_top ).
    grid = NEW #( i_parent = container ).

    CASE me->gv_operation_type.

      WHEN me->gc_option_type-report.
        gt_list = NEW priority( )->get_report_list( ).
        IF lines( gt_list ) = 0.
          LEAVE TO SCREEN 0.
        ENDIF.

        DATA(lt_sort) = VALUE lvc_t_sort( up    = 'X'
                                          group = 'X'
                                          ( spos      = 01
                                            fieldname = 'BC'                                           )
                                          ( spos      = 02
                                            fieldname = 'PRIORITY' ) ) .

      WHEN me->gc_option_type-maintain.
        SET HANDLER me->handle_grid_drag FOR grid.
        SET HANDLER me->handle_grid_drop FOR grid.
        SET HANDLER me->handle_grid_drop_complete FOR grid.

        TRY.
            gt_list = build_data( ).
          CATCH lcx_exception.
        ENDTRY.

      WHEN OTHERS.
        LEAVE TO SCREEN 0.

    ENDCASE.

    build_and_assign_handler( ).
    DATA(lt_fieldcatalog) = get_fieldcatalog( lc_structure ).
    grid->set_table_for_first_display( EXPORTING is_layout       = gs_layout
                                       CHANGING  it_fieldcatalog = lt_fieldcatalog
                                                 it_sort         = lt_sort
                                                 it_outtab       = gt_list[] ).
    grid->refresh_table_display( ).
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

    gs_layout-ctab_fname = 'CELLCOLOR' .
    gs_layout-zebra      = abap_on.
    gs_layout-cwidth_opt = abap_on.
    gs_layout-s_dragdrop-row_ddid = handle_grid.
  ENDMETHOD.

  METHOD build_data.
    DATA(object) = NEW priority( ).

    TRY.
        DATA(bc) = object->get_responsable( ).
      CATCH lcx_exception INTO DATA(lo_exception). " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
*        MESSAGE lo_exception->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
*PREVIOUS->GET_TEXT( )
*        RAISE EXCEPTION lo_exception->previous( ).
    ENDTRY.

    result = VALUE #( FOR l IN object->get_bc_list( bc )
                      ( CORRESPONDING #( l ) ) ).
  ENDMETHOD.

  METHOD get_fieldcatalog.
    DATA lt_fieldcatalog TYPE lvc_t_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name       = im_strutucre     " Structure name (structure, table, view)
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

  DATA(lt_excluded) = application->get_excluded( ).
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
      NEW priority( )->save( im_data = gt_list ).
      LEAVE PROGRAM.

    WHEN OTHERS.

  ENDCASE.

  CLEAR save_ok_code.

ENDMODULE.


INITIALIZATION.


START-OF-SELECTION.
  SET SCREEN 100.

  "
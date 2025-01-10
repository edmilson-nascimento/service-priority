REPORT priory.

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
      EXPORTING
        headdata            = me->gs_header
      IMPORTING
        return              = result
      TABLES
        materialdescription = me->gt_description.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_order_list DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_responsable
      RETURNING VALUE(result) TYPE zca_tquermessebc-bc.

    METHODS display_bc_list
      IMPORTING im_bc TYPE zca_tquermessebc-bc.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_bc,
        bc        TYPE zca_tquermessebc-bc,
        name_text TYPE adrp-name_text,
      END OF ty_bc,
      tab_bc TYPE STANDARD TABLE OF ty_bc WITH DEFAULT KEY,
      BEGIN OF ty_ordenation,
        inc          TYPE zca_tquermessebc-inc,
        descricao_oc TYPE zca_tquermessebc-descricao_oc,
        label_oc     TYPE zca_tquermessebc-label_oc,
        bc           TYPE zca_tquermessebc-bc,
      END OF ty_ordenation,
      tab_ordenation TYPE STANDARD TABLE OF ty_ordenation WITH DEFAULT KEY.

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
      IMPORTING
        select_value              = select_value                " selected value
      TABLES
        fields                    = fields                 " internal table for transfer of the
        valuetab                  = valuetab                 " internal table for transfer of the
      EXCEPTIONS
        field_not_in_ddic         = 1                " Table field not listed in the Dict
        more_then_one_selectfield = 2                " During selection, only transfer of
        no_selectfield            = 3                " No field selected for transfer
        OTHERS                    = 4.
    IF sy-subrc <> 0.
      RETURN .
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
    cl_demo_output=>display( data =  im_data ) .

  ENDMETHOD.

ENDCLASS.


INITIALIZATION.

  DATA(object) = NEW lcl_order_list( ).

  " Verificar os BCs com Incidentes abertos
  DATA(bc) = object->get_responsable( ).
  IF bc IS INITIAL.
    RETURN.
  ENDIF.

  object->display_bc_list( bc ).

  " Listar os Incidentes do BC






  "
```abap
************************************************************************
* Programa..: ZREP_RECONC_ESTOQUE
* Autor.....: ChatGPT (OpenAI)
* Data......: 2024-06-12
* Descrição.: Relatório de Reconciliação de Estoque (MARD x MSEG)
************************************************************************

REPORT zrep_reconc_estoque.

*-----------------------------------------------------------------------
* Tipos Globais
*-----------------------------------------------------------------------
TYPES: BEGIN OF ty_mard,
         matnr TYPE mard-matnr,
         werks TYPE mard-werks,
         lgort TYPE mard-lgort,
         labst TYPE mard-labst,
       END OF ty_mard.

TYPES: BEGIN OF ty_mseg_sum,
         matnr TYPE mseg-matnr,
         werks TYPE mseg-werks,
         lgort TYPE mseg-lgort,
         qty_mov TYPE mseg-menge,
       END OF ty_mseg_sum.

TYPES: BEGIN OF ty_result,
         matnr         TYPE matnr,
         werks         TYPE werks_d,
         lgort         TYPE lgort_d,
         labst         TYPE labst,
         qty_mov       TYPE labst,
         diff          TYPE labst,
       END OF ty_result.

TYPES: tt_result TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.

*-----------------------------------------------------------------------
* Parâmetros de Seleção
*-----------------------------------------------------------------------
PARAMETERS: p_werks TYPE mard-werks OBLIGATORY,
            p_lgort TYPE mard-lgort OPTIONAL.

SELECT-OPTIONS: s_matnr FOR mard-matnr.

*-----------------------------------------------------------------------
* Classe Principal
*-----------------------------------------------------------------------
CLASS lcl_reconc_estoque DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_werks TYPE werks_d
          iv_lgort TYPE lgort_d OPTIONAL
          it_matnr TYPE RANGE OF matnr,

      execute,
      display_alv.

  PRIVATE SECTION.
    DATA:
      mv_werks TYPE werks_d,
      mv_lgort TYPE lgort_d,
      mt_matnr TYPE RANGE OF matnr,
      mt_result TYPE tt_result.

    METHODS:
      check_authority,
      get_mard_data
        RETURNING VALUE(rt_mard) TYPE STANDARD TABLE OF ty_mard,
      get_mseg_sum
        RETURNING VALUE(rt_mseg_sum) TYPE STANDARD TABLE OF ty_mseg_sum,
      build_result
        IMPORTING
          it_mard     TYPE STANDARD TABLE OF ty_mard
          it_mseg_sum TYPE STANDARD TABLE OF ty_mseg_sum
        RETURNING VALUE(rt_result) TYPE tt_result.
ENDCLASS.

*-----------------------------------------------------------------------
* Implementação da Classe Principal
*-----------------------------------------------------------------------
CLASS lcl_reconc_estoque IMPLEMENTATION.

  METHOD constructor.
    mv_werks = iv_werks.
    mv_lgort = iv_lgort.
    mt_matnr = it_matnr.
  ENDMETHOD.

  METHOD execute.
    TRY.
        check_authority( ).
        DATA(lt_mard) = get_mard_data( ).
        DATA(lt_mseg_sum) = get_mseg_sum( ).
        mt_result = build_result(
          it_mard     = lt_mard
          it_mseg_sum = lt_mseg_sum
        ).
        display_alv( ).
      CATCH cx_root INTO DATA(lo_err).
        MESSAGE lo_err->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD check_authority.
    "Verifica autorização para visualizar dados de estoque
    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
      ID 'WERKS' FIELD mv_werks
      ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_no_authority
        EXPORTING
          textid = cx_no_authority=>not_authorized.
    ENDIF.
  ENDMETHOD.

  METHOD get_mard_data.
    "Busca dados de estoque atual (MARD)
    SELECT matnr, werks, lgort, labst
      FROM mard
      WHERE werks = @mv_werks
        AND ( @mt_matnr IS INITIAL OR matnr IN @mt_matnr )
        AND ( @mv_lgort IS INITIAL OR lgort = @mv_lgort )
      INTO TABLE @DATA(lt_mard).
    rt_mard = lt_mard.
  ENDMETHOD.

  METHOD get_mseg_sum.
    "Busca soma dos movimentos de material (MSEG) para o estoque
    SELECT matnr, werks, lgort, SUM( menge ) AS qty_mov
      FROM mseg
      WHERE werks = @mv_werks
        AND ( @mt_matnr IS INITIAL OR matnr IN @mt_matnr )
        AND ( @mv_lgort IS INITIAL OR lgort = @mv_lgort )
        AND bwart IN ( '101', '102', '201', '202', '261', '262', '301', '302', '311', '312', '601', '602' )
      GROUP BY matnr, werks, lgort
      INTO TABLE @DATA(lt_mseg_sum).
    rt_mseg_sum = lt_mseg_sum.
  ENDMETHOD.

  METHOD build_result.
    "Monta resultado da reconciliação
    rt_result = VALUE #( ).
    LOOP AT it_mard ASSIGNING FIELD-SYMBOL(<ls_mard>).
      DATA(ls_mseg_sum) = VALUE ty_mseg_sum(
        qty_mov = 0
      ).
      READ TABLE it_mseg_sum INTO ls_mseg_sum
        WITH KEY matnr = <ls_mard>-matnr
                 werks = <ls_mard>-werks
                 lgort = <ls_mard>-lgort.
      " Calcula diferença entre estoque e movimentos
      APPEND VALUE ty_result(
        matnr   = <ls_mard>-matnr
        werks   = <ls_mard>-werks
        lgort   = <ls_mard>-lgort
        labst   = <ls_mard>-labst
        qty_mov = ls_mseg_sum-qty_mov
        diff    = <ls_mard>-labst - ls_mseg_sum-qty_mov
      ) TO rt_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD display_alv.
    "Exibe o resultado em ALV usando CL_SALV_TABLE
    DATA(lo_alv) = NEW cl_salv_table( ).
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = mt_result
        ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->get_column( 'MATNR' )->set_long_text( 'Material' ).
        lo_alv->get_columns( )->get_column( 'WERKS' )->set_long_text( 'Centro' ).
        lo_alv->get_columns( )->get_column( 'LGORT' )->set_long_text( 'Depósito' ).
        lo_alv->get_columns( )->get_column( 'LABST' )->set_long_text( 'Estoque Atual' ).
        lo_alv->get_columns( )->get_column( 'QTY_MOV' )->set_long_text( 'Quantidade Movimentada' ).
        lo_alv->get_columns( )->get_column( 'DIFF' )->set_long_text( 'Diferença' ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO DATA(lo_msg).
        MESSAGE lo_msg->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*-----------------------------------------------------------------------
* Bloco de Execução
*-----------------------------------------------------------------------
START-OF-SELECTION.
  DATA(lo_report) = NEW lcl_reconc_estoque(
    iv_werks = p_werks
    iv_lgort = p_lgort
    it_matnr = s_matnr[]
  ).
  lo_report->execute( ).
```
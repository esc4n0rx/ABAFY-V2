*&---------------------------------------------------------------------*
*& Report ZR_CONSULTA_MATERIAIS
*&---------------------------------------------------------------------*
*& Descrição: Consulta de materiais com dados das tabelas MARA e MAKT
*&---------------------------------------------------------------------*
REPORT zr_consulta_materiais.

************************************************************************
* Tipos e Estruturas
************************************************************************
TYPES: BEGIN OF ty_material,
         matnr TYPE mara-matnr,
         maktx TYPE makt-maktx,
         mtart TYPE mara-mtart,
         matkl TYPE mara-matkl,
         mstae TYPE mara-mstae,
         ersda TYPE mara-ersda,
       END OF ty_material.

DATA: gt_materiais TYPE TABLE OF ty_material,
      gv_count     TYPE i.

************************************************************************
* Tela de Seleção
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_matnr TYPE mara-matnr.
  SELECT-OPTIONS: s_matnr FOR mara-matnr.
  PARAMETERS: p_mtart TYPE mara-mtart,
              p_bloq  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* Classe de Implementação
************************************************************************
CLASS lcl_material_report DEFINITION.
  PUBLIC SECTION.
    METHODS:
      validate_selection RETURNING VALUE(rv_valid) TYPE abap_bool,
      get_data,
      display_alv.

  PRIVATE SECTION.
    METHODS:
      build_fieldcat RETURNING VALUE(rt_fieldcat) TYPE slis_t_fieldcat_alv.
ENDCLASS.

CLASS lcl_material_report IMPLEMENTATION.
  METHOD validate_selection.
    " Verifica se ao menos um critério de seleção foi informado
    IF p_matnr IS INITIAL AND s_matnr[] IS INITIAL AND p_mtart IS INITIAL.
      MESSAGE 'Informe ao menos um critério de seleção' TYPE 'E'.
      rv_valid = abap_false.
    ELSE.
      rv_valid = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_data.
    " Limpa tabela de resultados
    CLEAR: gt_materiais, gv_count.

    " Monta a consulta SQL
    SELECT mara~matnr, makt~maktx, mara~mtart, mara~matkl, 
           mara~mstae, mara~ersda
      INTO CORRESPONDING FIELDS OF TABLE @gt_materiais
      FROM mara
      LEFT JOIN makt ON makt~matnr = mara~matnr
                     AND makt~spras = 'P'
      WHERE ( mara~matnr = @p_matnr OR @p_matnr IS INITIAL )
        AND mara~matnr IN @s_matnr
        AND ( mara~mtart = @p_mtart OR @p_mtart IS INITIAL )
        AND ( mara~mstae <> 'X' OR @p_bloq = abap_true ).

    " Conta registros encontrados
    gv_count = lines( gt_materiais ).

    " Ordena por número do material
    SORT gt_materiais BY matnr.
  ENDMETHOD.

  METHOD display_alv.
    " Verifica se há dados para exibir
    IF gt_materiais IS INITIAL.
      MESSAGE 'Nenhum material encontrado com os critérios informados' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " Prepara estrutura para ALV
    DATA: ls_layout TYPE slis_layout_alv,
          lt_fieldcat TYPE slis_t_fieldcat_alv.

    " Configura layout
    ls_layout-zebra = abap_true.
    ls_layout-colwidth_optimize = abap_true.

    " Obtém catálogo de campos
    lt_fieldcat = build_fieldcat( ).

    " Exibe ALV com os resultados
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        is_layout          = ls_layout
        it_fieldcat        = lt_fieldcat
        i_save             = 'A'
      TABLES
        t_outtab           = gt_materiais
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD build_fieldcat.
    " Cria catálogo de campos para ALV
    DATA: ls_fieldcat TYPE slis_fieldcat_alv.

    " Campo: Número do Material
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MATNR'.
    ls_fieldcat-seltext_m = 'Número do Material'.
    ls_fieldcat-key       = abap_true.
    APPEND ls_fieldcat TO rt_fieldcat.

    " Campo: Descrição do Material
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MAKTX'.
    ls_fieldcat-seltext_m = 'Descrição do Material'.
    APPEND ls_fieldcat TO rt_fieldcat.

    " Campo: Tipo de Material
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MTART'.
    ls_fieldcat-seltext_m = 'Tipo de Material'.
    APPEND ls_fieldcat TO rt_fieldcat.

    " Campo: Grupo de Mercadorias
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MATKL'.
    ls_fieldcat-seltext_m = 'Grupo de Mercadorias'.
    APPEND ls_fieldcat TO rt_fieldcat.

    " Campo: Status do Material
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MSTAE'.
    ls_fieldcat-seltext_m = 'Status do Material'.
    APPEND ls_fieldcat TO rt_fieldcat.

    " Campo: Data de Criação
    CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ERSDA'.
    ls_fieldcat-seltext_m = 'Data de Criação'.
    APPEND ls_fieldcat TO rt_fieldcat.
  ENDMETHOD.
ENDCLASS.

************************************************************************
* Programa Principal
************************************************************************
INITIALIZATION.
  " Inicialização do programa

START-OF-SELECTION.
  " Instancia classe de processamento
  DATA(go_report) = NEW lcl_material_report( ).

  " Valida seleção
  IF go_report->validate_selection( ) = abap_false.
    RETURN.
  ENDIF.

  " Obtém dados
  go_report->get_data( ).

  " Exibe resultado
  go_report->display_alv( ).

END-OF-SELECTION.
  " Exibe contador de registros
  IF gv_count > 0.
    WRITE: / |{ gv_count } materiais encontrados.|.
  ENDIF.
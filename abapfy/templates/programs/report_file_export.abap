*&---------------------------------------------------------------------*
*& Report Z_NOME_DO_PROGRAMA_EXPORT
*&---------------------------------------------------------------------*
*& Autor: {* NOME DO AUTOR (via ABAPFY) *}
*& Data:  {* DATA ATUAL (via ABAPFY) *}
*& Descrição: {* Extração de dados para arquivo local... *}
*&---------------------------------------------------------------------*

REPORT z_nome_do_programa_export.

TYPES:
  BEGIN OF ty_export_data,
    {* DEFINA OS CAMPOS PARA EXPORTAÇÃO *}
  END OF ty_export_data.

DATA:
  gt_data TYPE TABLE OF ty_export_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_key FOR {* CAMPO CHAVE *}.
  PARAMETERS: pa_path TYPE string LOWER CASE OBLIGATORY DEFAULT 'C:\temp\export.csv'.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_export DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      run.
  PRIVATE SECTION.
    DATA:
      mv_file_path TYPE string.
    METHODS:
      get_data,
      export_to_csv.
    CONSTRUCTOR
      IMPORTING
        iv_file_path TYPE string.
ENDCLASS.

CLASS lcl_export IMPLEMENTATION.
  METHOD constructor.
    mv_file_path = iv_file_path.
  ENDMETHOD.

  METHOD run.
    get_data( ).
    export_to_csv( ).
  ENDMETHOD.

  METHOD get_data.
    SELECT
      {* LISTE OS CAMPOS DO BANCO *}
    FROM {* NOME DA TABELA *}
    WHERE {* CAMPO CHAVE *} IN @so_key
    INTO TABLE @gt_data.

    IF gt_data IS INITIAL.
      MESSAGE 'Nenhum dado encontrado para exportação.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.

  METHOD export_to_csv.
    DATA:
      lt_string_table TYPE TABLE OF string,
      lv_header       TYPE string.

    "--- Monta o cabeçalho do CSV
    lv_header = '{* NOME_CAMPO_1;NOME_CAMPO_2;... *}'.
    APPEND lv_header TO lt_string_table.

    "--- Converte dados para string separada por ponto e vírgula
    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      DATA(lv_line) = |{ <fs_data>-field1 }|
                   && |;|
                   && |{ <fs_data>-field2 }|.
                   " ... adicione todos os campos
      APPEND lv_line TO lt_string_table.
    ENDLOOP.

    "--- Usa GUI_DOWNLOAD para salvar o arquivo
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename = mv_file_path
        filetype = 'ASC' " ASCII
      CHANGING
        data_tab = lt_string_table
      EXCEPTIONS
        OTHERS   = 4 ).

    IF sy-subrc = 0.
      MESSAGE |Arquivo salvo com sucesso em: { mv_file_path }| TYPE 'S'.
    ELSE.
      MESSAGE 'Erro ao salvar o arquivo.' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  TEXT-001 = 'Critérios de Seleção'.

START-OF-SELECTION.
  NEW lcl_export( iv_file_path = pa_path )->run( ).
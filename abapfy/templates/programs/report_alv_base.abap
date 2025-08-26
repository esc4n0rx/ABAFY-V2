*&---------------------------------------------------------------------*
*& Report Z_NOME_DO_PROGRAMA
*&---------------------------------------------------------------------*
*& Autor: {* NOME DO AUTOR (via ABAPFY) *}
*& Data:  {* DATA ATUAL (via ABAPFY) *}
*& Descrição: {* DESCRIÇÃO FORNECIDA PELO USUÁRIO *}
*&---------------------------------------------------------------------*

REPORT z_nome_do_programa.

TYPES:
  BEGIN OF ty_output,
    {* INCLUA OS CAMPOS DA ESTRUTURA DE SAÍDA AQUI *}
    field1 TYPE c,
    field2 TYPE i,
  END OF ty_output.

DATA:
  gt_output TYPE TABLE OF ty_output.

*&---------------------------------------------------------------------*
*& Tela de Seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  {* ADICIONE OS PARÂMETROS E SELECT-OPTIONS AQUI *}
  SELECT-OPTIONS: so_field FOR gt_output-field1.
SELECTION-SCREEN END OF BLOCK b1.


*&---------------------------------------------------------------------*
*& Classe Local para Lógica do Relatório
*&---------------------------------------------------------------------*
CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      run.

  PRIVATE SECTION.
    METHODS:
      get_data,
      process_data,
      display_alv.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run.
    get_data( ).
    process_data( ).
    display_alv( ).
  ENDMETHOD.

  METHOD get_data.
    " Busca os dados do banco de dados
    SELECT
      {* LISTE OS CAMPOS DO BANCO DE DADOS AQUI *}
      field1,
      field2
    FROM {* NOME DA TABELA PRINCIPAL *}
    WHERE field1 IN @so_field
    INTO TABLE @gt_output.

    IF sy-subrc <> 0.
      MESSAGE 'Nenhum dado encontrado.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.

  METHOD process_data.
    " Se necessário, processe os dados (cálculos, formatações, etc.)
    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<fs_output>).
      {* IMPLEMENTE A LÓGICA DE PROCESSAMENTO AQUI *}
    ENDLOOP.
  ENDMETHOD.

  METHOD display_alv.
    " Exibe os dados usando a classe CL_SALV_TABLE
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = gt_output
        ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        MESSAGE lx_salv_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*& Eventos do Programa
*&---------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Critérios de Seleção'.

START-OF-SELECTION.
  NEW lcl_report( )->run( ).
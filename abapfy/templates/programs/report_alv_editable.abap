*&---------------------------------------------------------------------*
*& Report Z_NOME_DO_PROGRAMA_EDIT
*&---------------------------------------------------------------------*
*& Autor: {* NOME DO AUTOR (via ABAPFY) *}
*& Data:  {* DATA ATUAL (via ABAPFY) *}
*& Descrição: {* Relatório para edição em massa de dados... *}
*&---------------------------------------------------------------------*

REPORT z_nome_do_programa_edit.

TYPES:
  BEGIN OF ty_editable_data,
    key_field    TYPE {* TIPO DO CAMPO CHAVE *},
    editable_field TYPE {* TIPO DO CAMPO EDITÁVEL *},
    read_only_field TYPE {* TIPO DO CAMPO NÃO EDITÁVEL *},
    style        TYPE salv_t_ui_color_cells, " Coluna para estilo das células
  END OF ty_editable_data.

DATA:
  gt_data TYPE TABLE OF ty_editable_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_key FOR gt_data-key_field.
SELECTION-SCREEN END OF BLOCK b1.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_salv_events_table
        IMPORTING
          er_data_changed.
ENDCLASS.

CLASS lcl_report DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: run.
  PRIVATE SECTION.
    METHODS:
      get_data,
      display_alv.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run.
    get_data( ).
    display_alv( ).
  ENDMETHOD.

  METHOD get_data.
    SELECT
      {* CAMPO_CHAVE, CAMPO_EDITAVEL, CAMPO_NAO_EDITAVEL *}
    FROM {* NOME DA TABELA *}
    WHERE key_field IN @so_key
    INTO TABLE @gt_data.

    IF sy-subrc <> 0.
      MESSAGE 'Nenhum dado encontrado.' TYPE 'S'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.

  METHOD display_alv.
    DATA(lo_handler) = NEW lcl_handler( ).

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                               CHANGING  t_table      = gt_data ).

        "--- Configurações do ALV
        lo_alv->get_functions( )->set_all( ).
        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

        "--- Habilita modo de edição
        DATA(lo_settings) = lo_alv->get_display_settings( ).
        lo_settings->set_edit_mode( if_salv_c_edit_mode=>mass ).

        "--- Define colunas editáveis e não editáveis
        DATA(lo_column_key) = lo_alv->get_columns( )->get_column( '{* NOME_COLUNA_CHAVE *}' ).
        lo_column_key->set_cell_type( if_salv_c_cell_type=>text ).
        lo_column_key->set_read_only( abap_true ).

        DATA(lo_column_editable) = lo_alv->get_columns( )->get_column( '{* NOME_COLUNA_EDITAVEL *}' ).
        lo_column_editable->set_cell_type( if_salv_c_cell_type=>hotspot ). " ou outro tipo

        "--- Registra evento para capturar mudanças
        SET HANDLER lo_handler->on_data_changed FOR lo_alv->get_event( ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv_msg).
        MESSAGE lx_salv_msg TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_data_changed.
    "--- Lógica para tratar os dados alterados
    " er_data_changed->mt_modified_cells contém as células modificadas
    MESSAGE 'Dados foram alterados. Implemente a lógica de gravação.' TYPE 'S'.

    " Exemplo de como atualizar a tabela interna
    LOOP AT er_data_changed->mt_modified_cells ASSIGNING FIELD-SYMBOL(<fs_mod>).
      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) INDEX <fs_mod>-row.
      IF sy-subrc = 0.
        ASSIGN COMPONENT <fs_mod>-colname OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_field>).
        IF sy-subrc = 0.
          <fs_field> = <fs_mod>-value.
        ENDIF.
      ENDIF.
    ENDLOOP.

    " Aqui você chamaria um método para salvar as alterações no banco de dados.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  TEXT-001 = 'Critérios de Seleção'.

START-OF-SELECTION.
  NEW lcl_report( )->run( ).
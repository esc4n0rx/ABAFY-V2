METHOD select_data.
  "--- Validação dos parâmetros de entrada
  IF iv_key IS INITIAL.
    RAISE EXCEPTION TYPE cx_sy_illegal_argument.
  ENDIF.

  "--- Limpa a tabela de retorno
  CLEAR et_data.

  "--- Seleciona os dados da tabela
  SELECT
    {* LISTE OS CAMPOS A SEREM SELECIONADOS *}
  FROM {* NOME DA TABELA *}
  WHERE {* CONDIÇÃO WHERE BASEADA NOS PARÂMETROS DE IMPORTAÇÃO *} = @iv_key
  INTO TABLE @et_data.
  " Adicionar FOR ALL ENTRIES se necessário

  " Lançar exceção se nenhum dado for encontrado (opcional, dependendo do requisito)
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_select_error
      EXPORTING
        textid = cx_sy_select_error=>no_entry_found.
  ENDIF.

ENDMETHOD.
METHOD update_data.
  "--- Validação dos dados de entrada
  IF is_data IS INITIAL.
    RAISE EXCEPTION TYPE cx_sy_illegal_argument.
  ENDIF.
  
  "--- Verificação de autorização antes de modificar
  AUTHORITY-CHECK OBJECT '{* OBJETO DE AUTORIZAÇÃO *}'
    ID 'ACTVT' FIELD '02' " 02 = Change
    ID '{* OUTRO CAMPO DE AUTORIZAÇÃO *}' FIELD is_data-key_field.
  
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE cx_sy_authorization_error.
  ENDIF.
  
  "--- Lógica para modificar a tabela Z
  UPDATE {* NOME DA TABELA Z *} SET
    {* LISTA DE CAMPOS A SEREM ATUALIZADOS (SET field1 = @is_data-field1, ...) *}
  WHERE {* CONDIÇÃO WHERE PARA A CHAVE *}.

  IF sy-subrc <> 0.
    " Falha na atualização, lança exceção
    RAISE EXCEPTION TYPE cx_sy_update_error.
  ELSE.
    " Sucesso, retorna os dados atualizados
    rs_updated_data = is_data.
  ENDIF.

ENDMETHOD.
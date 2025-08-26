"--- Verificação de autorização para a atividade {* Descrever a atividade *}
AUTHORITY-CHECK OBJECT '{* NOME DO OBJETO DE AUTORIZAÇÃO, ex: S_CARRID *}'
  ID 'ACTVT'  FIELD '{* ATIVIDADE, ex: 03 para Display *}'
  ID 'CARRID' FIELD {* VARIÁVEL COM O VALOR A SER CHECADO *}.

IF sy-subrc <> 0.
  " Sem autorização, lançar exceção ou emitir mensagem de erro
  RAISE EXCEPTION TYPE cx_sy_authorization_error
    EXPORTING
      textid = cx_sy_authorization_error=>no_authorization.
ENDIF.
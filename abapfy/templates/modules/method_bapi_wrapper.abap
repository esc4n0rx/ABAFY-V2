METHOD call_bapi_wrapper.
  DATA:
    lt_return TYPE TABLE OF bapiret2.

  "--- Chamada da BAPI
  CALL FUNCTION '{* NOME_DA_BAPI *}'
    EXPORTING
      {* Parâmetros de EXPORTING *}
    IMPORTING
      {* Parâmetros de IMPORTING *}
    TABLES
      return = lt_return
      {* Outras tabelas *}.

  "--- Tratamento de mensagens de retorno
  LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_return>)
                   WHERE type = 'E' OR type = 'A'.
    " Erro encontrado, lança exceção com a mensagem da BAPI
    RAISE EXCEPTION TYPE {* CX_NOME_DA_EXCECAO_CUSTOMIZADA *}
      EXPORTING
        textid = VALUE scx_t100key( msgid = <fs_return>-id
                                   msgno = <fs_return>-number
                                   attr1 = <fs_return>-message_v1
                                   attr2 = <fs_return>-message_v2
                                   attr3 = <fs_return>-message_v3
                                   attr4 = <fs_return>-message_v4 ).
  ENDLOOP.

  "--- Lógica de commit (se necessário)
  IF commit_work = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDIF.

  "--- Atribui os resultados
  es_result = {* Estrutura de resultado preenchida com os dados do IMPORTING *}.

ENDMETHOD.
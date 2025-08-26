DATA:
  ls_log_handle TYPE balloghndl,
  ls_log        TYPE bal_s_log.

"--- 1. Define o cabeçalho do log
ls_log-object    = '{* Z_OBJETO_LOG *}'.
ls_log-subobject = '{* Z_SUBOBJETO_LOG *}'.
ls_log-extnumber = '{* NÚMERO EXTERNO / IDENTIFICADOR DO PROCESSO *}'.

cl_bali_log=>create(
  EXPORTING
    i_log      = ls_log
  IMPORTING
    e_log_handle = ls_log_handle
).

"--- 2. Adiciona mensagens ao log
cl_bali_message_setter=>add_message(
  i_log_handle = ls_log_handle
  i_msg_type   = '{* S, E, W, I, A *}'
  i_msg_id     = '{* CLASSE DE MENSAGEM *}'
  i_msg_number = '{* NÚMERO DA MENSAGEM *}'
  i_msg_v1     = {* VARIÁVEL 1 *}
).

"--- Repita o passo 2 para quantas mensagens precisar

"--- 3. Salva o log no banco de dados
cl_bali_log=>save(
  EXPORTING
    i_log_handle = ls_log_handle
).
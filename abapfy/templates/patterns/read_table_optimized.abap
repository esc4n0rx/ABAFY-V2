"--- Leitura otimizada de tabela interna
TRY.
    " Use table expression para uma leitura limpa e direta
    DATA(ls_found_data) = gt_tabela_interna[ {* CAMPO CHAVE *} = lv_valor_chave ].

    " Continue a lógica com ls_found_data...

  CATCH cx_sy_itab_line_not_found.
    " Tratamento para o caso de a linha não ser encontrada
    MESSAGE 'Registro não encontrado na tabela interna.' TYPE 'E'.
ENDTRY.
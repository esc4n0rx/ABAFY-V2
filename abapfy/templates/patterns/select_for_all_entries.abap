"--- Garante que a tabela de drivers não está vazia para evitar full table scan
IF gt_source_data IS NOT INITIAL.

  "--- Ordena e remove duplicatas da chave para otimização
  SORT gt_source_data BY key_field.
  DELETE ADJACENT DUPLICATES FROM gt_source_data COMPARING key_field.

  "--- Seleção performática usando FOR ALL ENTRIES
  SELECT
    {* LISTE OS CAMPOS NECESSÁRIOS *}
  FROM {* NOME DA TABELA ALVO *}
  FOR ALL ENTRIES IN @gt_source_data
  WHERE key_field = @gt_source_data-key_field
  INTO TABLE @gt_result_data.

ENDIF.
```abap
  METHOD get_customer_data.
    "----------------------------------------------------------------------
    " Método        : GET_CUSTOMER_DATA
    " Descrição     : Retorna os dados do cliente (KNA1) para um número de cliente informado.
    " Parâmetros
    "   IMPORTING
    "     iv_kunnr TYPE kunnr - Número do cliente a ser consultado
    "   EXPORTING
    "     rs_kna1  TYPE kna1  - Estrutura com os dados do cliente
    " Exceções tratadas:
    "   - Número de cliente não informado ou inválido
    "   - Cliente não encontrado no banco de dados
    "----------------------------------------------------------------------
    
    " Limpa a estrutura de saída
    CLEAR rs_kna1.
    
    " Validação do parâmetro de entrada
    IF iv_kunnr IS INITIAL.
      " Número do cliente não informado
      RAISE EXCEPTION TYPE cx_sy_argument_error
        EXPORTING
          textid = cx_sy_argument_error=>argument_not_found
          previous = CONV #( 'Número do cliente (KUNNR) não informado.' ).
      RETURN.
    ENDIF.

    " Seleção dos dados do cliente na tabela KNA1
    SELECT SINGLE *
      FROM kna1
      WHERE kunnr = @iv_kunnr
      INTO @rs_kna1.

    IF sy-subrc <> 0.
      " Cliente não encontrado
      RAISE EXCEPTION TYPE cx_sy_record_not_found
        EXPORTING
          textid = cx_sy_record_not_found=>not_found
          previous = CONV #( 'Cliente não encontrado na tabela KNA1.' ).
      RETURN.
    ENDIF.

    " Dados do cliente encontrados e preenchidos em rs_kna1

  ENDMETHOD.
```
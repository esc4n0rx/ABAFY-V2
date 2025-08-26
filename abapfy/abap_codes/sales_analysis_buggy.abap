*&---------------------------------------------------------------------*
*& Report ZTEST_SALES_ANALYSIS
*&---------------------------------------------------------------------*
*& Programa: Análise de Vendas por Cliente
*& Autor: Dev Team
*& Data: 26/08/2025
*& Descrição: Relatório que analisa vendas por cliente com cálculos
*&            de comissões e análise de performance
*&---------------------------------------------------------------------*

REPORT ztest_sales_analysis.

"=======================================================================
" DECLARAÇÕES GLOBAIS
"=======================================================================
TYPES: BEGIN OF ty_sales_data,
         kunnr TYPE kunnr,        " Número do cliente
         name1 TYPE name1_gp,     " Nome do cliente
         vbeln TYPE vbeln_va,     " Número do documento de venda
         netwr TYPE netwr_ak,     " Valor líquido
         waerk TYPE waers,        " Moeda
         erdat TYPE erdat,        " Data de criação
         vkorg TYPE vkorg,        " Organização de vendas
       END OF ty_sales_data.

TYPES: tt_sales_data TYPE TABLE OF ty_sales_data.

TYPES: BEGIN OF ty_commission,
         kunnr TYPE kunnr,
         total_sales TYPE netwr_ak,
         commission_rate TYPE p DECIMALS 2,
         commission_value TYPE p DECIMALS 2,
       END OF ty_commission.

TYPES: tt_commission TYPE TABLE OF ty_commission.

"=======================================================================
" TELA DE SELEÇÃO
"=======================================================================
SELECTION-SCREEN BEGIN OF BLOCK b001 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_kunnr FOR kna1-kunnr,
                  s_erdat FOR vbak-erdat DEFAULT sy-datum.
  PARAMETERS: p_vkorg TYPE vkorg DEFAULT '1000',
              p_waers TYPE waers DEFAULT 'BRL'.
SELECTION-SCREEN END OF BLOCK b001.

SELECTION-SCREEN BEGIN OF BLOCK b002 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_comm TYPE p DECIMALS 2 DEFAULT '5.00',
              p_bonus TYPE p DECIMALS 2 DEFAULT '1000.00',
              p_detail TYPE abap_bool DEFAULT abap_true AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b002.

"=======================================================================
" CLASSE LOCAL PARA ANÁLISE DE VENDAS
"=======================================================================
CLASS lcl_sales_analyzer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor,
             execute_analysis,
             display_results.
    
  PRIVATE SECTION.
    DATA: gt_sales_data TYPE tt_sales_data,
          gt_commissions TYPE tt_commission,
          gv_total_sales TYPE netwr_ak,
          gv_avg_sales TYPE p DECIMALS 2.
    
    METHODS: load_sales_data,
             calculate_commissions,
             calculate_statistics,
             validate_input_data,
             format_currency 
               IMPORTING iv_amount TYPE netwr_ak
                         iv_currency TYPE waers
               RETURNING VALUE(rv_formatted) TYPE string,
             get_commission_rate
               IMPORTING iv_customer TYPE kunnr
                         iv_total_sales TYPE netwr_ak
               RETURNING VALUE(rv_rate) TYPE p.
ENDCLASS.

CLASS lcl_sales_analyzer IMPLEMENTATION.
  METHOD constructor.
    " Inicialização básica da classe
    CLEAR: gt_sales_data, gt_commissions, gv_total_sales, gv_avg_sales.
  ENDMETHOD.

  METHOD execute_analysis.
    " Fluxo principal da análise
    TRY.
        validate_input_data( ).
        load_sales_data( ).
        calculate_commissions( ).
        calculate_statistics( ).
        
      CATCH cx_sy_zerodivide INTO DATA(lx_zero_div).
        MESSAGE lx_zero_div->get_text( ) TYPE 'E'.
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE 'Erro durante análise de vendas' TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_input_data.
    " BUG 1: Validação incompleta - não verifica todos os campos obrigatórios
    IF p_vkorg IS INITIAL.
      MESSAGE 'Organização de vendas deve ser informada' TYPE 'E'.
    ENDIF.
    
    " BUG 2: Não valida se a taxa de comissão é negativa
    IF p_comm > 100.
      MESSAGE 'Taxa de comissão não pode ser maior que 100%' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD load_sales_data.
    " BUG 3: SELECT * proibido - má prática de performance
    SELECT vbak~vbeln,
           vbak~kunnr,
           vbak~netwr,
           vbak~waerk,
           vbak~erdat,
           vbak~vkorg,
           kna1~name1
      FROM vbak
      INNER JOIN kna1 ON kna1~kunnr = vbak~kunnr
      INTO CORRESPONDING FIELDS OF TABLE @gt_sales_data
      WHERE vbak~kunnr IN @s_kunnr
        AND vbak~erdat IN @s_erdat
        AND vbak~vkorg = @p_vkorg
        AND vbak~waerk = @p_waers.

    IF sy-subrc <> 0.
      MESSAGE 'Nenhum dado de vendas encontrado' TYPE 'I'.
    ENDIF.

    " BUG 4: Não verifica se a tabela está vazia antes de processar
    SORT gt_sales_data BY kunnr netwr DESCENDING.
  ENDMETHOD.

  METHOD calculate_commissions.
    DATA: lv_commission_rate TYPE p DECIMALS 2,
          lv_customer_total TYPE netwr_ak,
          ls_commission TYPE ty_commission.

    " BUG 5: Loop sem otimização - deveria agrupar por cliente primeiro
    LOOP AT gt_sales_data INTO DATA(ls_sales).
      " BUG 6: Lógica de agrupamento incorreta - soma todas as vendas
      lv_customer_total = lv_customer_total + ls_sales-netwr.
      
      lv_commission_rate = get_commission_rate( 
        iv_customer = ls_sales-kunnr 
        iv_total_sales = lv_customer_total
      ).
      
      ls_commission-kunnr = ls_sales-kunnr.
      ls_commission-total_sales = lv_customer_total.
      ls_commission-commission_rate = lv_commission_rate.
      " BUG 7: Divisão potencial por zero não tratada adequadamente
      ls_commission-commission_value = ( lv_customer_total * lv_commission_rate ) / 100.
      
      APPEND ls_commission TO gt_commissions.
      " BUG 8: Não limpa a variável para o próximo cliente
    ENDLOOP.

    " BUG 9: Remove duplicatas de forma ineficiente
    SORT gt_commissions BY kunnr.
    DELETE ADJACENT DUPLICATES FROM gt_commissions COMPARING kunnr.
  ENDMETHOD.

  METHOD calculate_statistics.
    DATA: lv_count TYPE i.
    
    " Calcula total de vendas
    LOOP AT gt_sales_data INTO DATA(ls_sales).
      gv_total_sales = gv_total_sales + ls_sales-netwr.
      lv_count = lv_count + 1.
    ENDLOOP.
    
    " BUG 10: Divisão por zero não tratada
    gv_avg_sales = gv_total_sales / lv_count.
    
    " BUG 11: Conversão de moeda ignorada - assume sempre a moeda padrão
    IF gv_avg_sales > p_bonus.
      WRITE: / 'Vendas médias acima do bônus!'.
    ENDIF.
  ENDMETHOD.

  METHOD get_commission_rate.
    " BUG 12: Lógica de comissão simplificada demais
    IF iv_total_sales > 50000.
      rv_rate = p_comm + 2. " Bônus de 2%
    ELSEIF iv_total_sales > 10000.
      rv_rate = p_comm + 1. " Bônus de 1%
    ELSE.
      rv_rate = p_comm.
    ENDIF.
    
    " BUG 13: Não considera limite máximo de comissão
  ENDMETHOD.

  METHOD format_currency.
    " BUG 14: Formatação sem considerar configurações locais
    DATA: lv_amount_str TYPE string.
    
    lv_amount_str = |{ iv_amount }|.
    rv_formatted = |{ lv_amount_str } { iv_currency }|.
    
    " BUG 15: Não trata casas decimais corretamente
  ENDMETHOD.

  METHOD display_results.
    DATA: lv_formatted_value TYPE string.
    
    WRITE: / 'RELATÓRIO DE ANÁLISE DE VENDAS',
           / sy-uline(50).
    
    WRITE: / 'Total de Vendas:', 
           / gv_total_sales, p_waers.
    
    WRITE: / 'Média de Vendas:',
           / gv_avg_sales, p_waers.
    
    " BUG 16: Não verifica se há dados antes de exibir
    IF p_detail = abap_true.
      WRITE: / sy-uline(50),
             / 'DETALHAMENTO POR CLIENTE:'.
      
      LOOP AT gt_commissions INTO DATA(ls_comm).
        lv_formatted_value = format_currency( 
          iv_amount = ls_comm-total_sales 
          iv_currency = p_waers 
        ).
        
        WRITE: / 'Cliente:', ls_comm-kunnr,
               / 'Total Vendas:', lv_formatted_value,
               / 'Taxa Comissão:', ls_comm-commission_rate, '%',
               / 'Valor Comissão:', ls_comm-commission_value, p_waers,
               / sy-uline(30).
      ENDLOOP.
    ENDIF.
    
    " BUG 17: Não mostra mensagem quando não há dados
  ENDMETHOD.
ENDCLASS.

"=======================================================================
" EVENTOS PRINCIPAIS
"=======================================================================
INITIALIZATION.
  " Textos da tela de seleção
  TEXT-001 = 'Seleção de Dados'.
  TEXT-002 = 'Parâmetros de Comissão'.

AT SELECTION-SCREEN.
  " BUG 18: Validação de tela incompleta
  IF s_erdat-high IS NOT INITIAL AND s_erdat-low > s_erdat-high.
    MESSAGE 'Data inicial não pode ser maior que data final' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  DATA: lo_analyzer TYPE REF TO lcl_sales_analyzer.
  
  " BUG 19: Não trata exceções durante criação da instância
  CREATE OBJECT lo_analyzer.
  
  lo_analyzer->execute_analysis( ).
  lo_analyzer->display_results( ).

END-OF-SELECTION.
  WRITE: / sy-uline(50),
         / 'Processamento concluído em:', sy-uzeit.
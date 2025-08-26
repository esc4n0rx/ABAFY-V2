*&---------------------------------------------------------------------*
*&  Classe ZCL_IM_NOME_DA_BADI
*&---------------------------------------------------------------------*
*&  Implementação da BAdI: {* NOME_DA_BADI *}
*&---------------------------------------------------------------------*
CLASS zcl_im_nome_da_badi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES {* NOME_DA_INTERFACE_DA_BADI, ex: if_ex_badi_name *}.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_im_nome_da_badi IMPLEMENTATION.
  METHOD {* NOME_DA_INTERFACE_DA_BADI~NOME_DO_METODO *}.
    "--------------------------------------------------------------------
    " LÓGICA DA BADI
    "--------------------------------------------------------------------
    " Descrição: {* Descrição da lógica a ser implementada *}
    "
    " Parâmetros disponíveis:
    " -> {* Liste os parâmetros de CHANGING/IMPORTING para referência *}
    "--------------------------------------------------------------------

    {* IMPLEMENTE AQUI A LÓGICA DA SUA BADI *}

    " Exemplo: Modificar um parâmetro de changing
    " cs_data-flag = abap_true.

  ENDMETHOD.
ENDCLASS.
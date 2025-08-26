FUNCTION zwm_etiqueta_pic.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_WERKS) TYPE  WERKS_D
*"     REFERENCE(I_NLENR) TYPE  LTAP_NLENR
*"     REFERENCE(I_NLPLA) TYPE  LTAP_NLPLA
*"     REFERENCE(I_TBNUM) TYPE  TBNUM OPTIONAL
*"----------------------------------------------------------------------
  DATA:
    lv_etiq TYPE  zstwmetiqueta_09.

  DATA: lt_t001w TYPE TABLE OF t001w,
        l_nlpla  TYPE  ltap_nlpla.


  IF sy-tcode = 'ZWM061' OR sy-tcode = 'ZWM077'.
* Centro
    lv_etiq-werks = i_nlpla.

  ELSE.
* Centro
    lv_etiq-werks = i_werks.

  ENDIF.


  DATA(vl_tamanho) = strlen( i_nlpla ).

  IF vl_tamanho > 4.

    l_nlpla = |{ i_nlpla ALPHA = IN }|.

    SELECT SINGLE name1
      FROM kna1 INTO lv_etiq-name1
      WHERE kunnr EQ l_nlpla.

  ELSE.

* Seleciona descrição da loja
    SELECT SINGLE name1
      FROM t001w INTO lv_etiq-name1
       WHERE werks = i_nlpla(4).

  ENDIF.

  lv_etiq-dtsep = sy-datum.
  lv_etiq-lenum = i_nlenr.
  lv_etiq-lgnum = i_lgnum.
  lv_etiq-tbnum = i_tbnum.

  SHIFT lv_etiq-lenum LEFT DELETING LEADING '0'.

* Imprime etiqueta
  CALL FUNCTION 'Z_WM_ZEBRA_WM09'
    EXPORTING
      i_etiq = lv_etiq.

ENDFUNCTION.
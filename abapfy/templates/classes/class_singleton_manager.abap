*&---------------------------------------------------------------------*
*&  Classe ZCL_SINGLETON_MANAGER
*&---------------------------------------------------------------------*
*&  Descrição: {* Gerenciador de... (Ex: Configurações, Logs) *}
*&---------------------------------------------------------------------*
CLASS zcl_singleton_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_singleton_manager.

    METHODS:
      {* Defina aqui os métodos públicos da instância *}
      do_something.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      go_instance TYPE REF TO zcl_singleton_manager.
    
    DATA:
      {* Defina aqui os atributos da instância *}
      mv_some_data TYPE string.

    METHODS:
      constructor.
ENDCLASS.

CLASS zcl_singleton_manager IMPLEMENTATION.
  METHOD get_instance.
    IF go_instance IS NOT BOUND.
      go_instance = NEW #( ).
    ENDIF.
    ro_instance = go_instance.
  ENDMETHOD.
  
  METHOD constructor.
    " Lógica de inicialização que deve rodar apenas uma vez
    {* Ex: Carregar configurações, inicializar buffers *}
    mv_some_data = 'Initialized'.
  ENDMETHOD.

  METHOD do_something.
    " Lógica de um método de instância
    WRITE: / 'Singleton instance method called. Data:', me->mv_some_data.
  ENDMETHOD.
ENDCLASS.
class ABAPPrompts:
    """
    Gerenciador de prompts refinados para geração de código e análise ABAP,
    com foco em práticas modernas (ABAP 7.4+), segurança e clareza.
    """

    @staticmethod
    def get_program_prompt(description: str, program_type: str = "REPORT") -> str:
        """
        Gera um prompt aprimorado para a criação de um programa ABAP completo.
        Instrui explicitamente o uso de classes locais e sintaxe moderna.
        """
        return f"""
Você é um engenheiro de software ABAP sênior, especialista em Clean Code e ABAP Moderno (7.5+).
Sua tarefa é gerar um programa ABAP do tipo '{program_type}' que seja completo, funcional e siga as melhores práticas do mercado.

DESCRIÇÃO FUNCIONAL:
{description}

DIRETRIZES TÉCNICAS OBRIGATÓRIAS:
- Sintaxe Moderna (ABAP 7.5+): Use expressões inline (DATA(...)), operadores de construção (VALUE, CONV), string templates, e expressões de tabela sempre que possível.
- Estrutura com Classes Locais: Proibido o uso de sub-rotinas (PERFORM). Toda a lógica de negócio e manipulação de dados deve ser encapsulada em uma ou mais classes locais (LCL).
- Tratamento de Exceções: Use exclusivamente o tratamento de exceções baseado em classes (TRY...CATCH...ENDTRY com classes CX_...). Não use sy-subrc para controle de fluxo de erros.
- Acesso a Dados: Os comandos SELECT devem listar os campos explicitamente. O uso de 'SELECT *' é estritamente proibido. Utilize a sintaxe moderna com '@' para escapar variáveis.
- Segurança: Inclua verificações de autorização (AUTHORITY-CHECK) quando o acesso a dados sensíveis for implícito na descrição.
- Nomenclatura: Siga rigorosamente as convenções de nomenclatura SAP (ex: GT_ para tabelas globais, LCL_ para classes locais, LO_ para instâncias, IV_ para importing, ES_ para exporting, RS_ para returning).
- Comentários: O código deve ser bem comentado em português, explicando a finalidade de blocos lógicos complexos.

ESTRUTURA DE CÓDIGO ESPERADA:
1. Cabeçalho do programa (com autor, data, descrição).
2. Declarações de tipos e dados globais (se estritamente necessário).
3. Tela de Seleção (PARAMETERS, SELECT-OPTIONS), se a descrição exigir.
4. Definição da Classe Local (CLASS lcl_... DEFINITION).
5. Implementação da Classe Local (CLASS lcl_... IMPLEMENTATION).
6. Bloco de Eventos (INITIALIZATION, AT SELECTION-SCREEN, START-OF-SELECTION) que instancia e executa a lógica da classe local.

O CÓDIGO DEVE SER COMPLETO, FUNCIONAL E PRONTO PARA SER COPIADO E COLADO EM UM AMBIENTE SAP.
Forneça apenas o código ABAP puro, sem explicações, markdown ou qualquer texto adicional.
"""

    @staticmethod
    def get_module_prompt(description: str, module_type: str) -> str:
        """
        Gera um prompt aprimorado para a criação de módulos ABAP (Métodos, Funções, etc.).
        Exige o uso de RETURNING para saídas únicas e tratamento de exceções moderno.
        """
        return f"""
Você é um arquiteto de software ABAP, especialista em design de APIs e código modular.
Crie um {module_type} ABAP seguindo a descrição funcional e as diretrizes técnicas abaixo.

DESCRIÇÃO FUNCIONAL:
{description}

DIRETRIZES TÉCNICAS OBRIGATÓRIAS:
- Assinatura Clara: Para MÉTODOS com um único parâmetro de saída, utilize 'RETURNING' em vez de 'EXPORTING'.
- Sintaxe Moderna (ABAP 7.4+): Utilize expressões e operadores modernos (VALUE, CONV, etc.) no corpo do módulo.
- Tratamento de Exceções: Implemente o tratamento de erros usando exclusivamente exceções baseadas em classes (RAISE EXCEPTION TYPE cx_...). Não retorne flags de erro ou sy-subrc. O `RETURN` após `RAISE EXCEPTION` é redundante e não deve ser usado.
- Validação de Entrada: Valide todos os parâmetros de importação no início do módulo para garantir que não são iniciais ou inválidos (fail-fast principle).
- Acesso a Dados: Se houver acesso ao banco de dados, os campos devem ser listados explicitamente no SELECT. 'SELECT *' é proibido.
- Documentação: O cabeçalho do módulo deve ser completo, documentando o propósito, todos os parâmetros (IMPORTING, EXPORTING, CHANGING, RETURNING) e as exceções que podem ser levantadas.
- Código Limpo: O módulo deve ter uma única responsabilidade. A lógica deve ser clara, bem estruturada e comentada em português.

Forneça apenas o snippet de código ABAP para o {module_type}, pronto para ser inserido em seu respectivo objeto (classe, grupo de funções, etc.).
"""

    @staticmethod
    def get_debug_prompt(code: str) -> str:
        """
        Gera um prompt refinado para depuração e análise de código ABAP.
        Adiciona critérios de modernização e segurança na análise.
        """
        return f"""
Você é um especialista sênior em debug, otimização e modernização de código ABAP.
Analise rigorosamente o seguinte código ABAP, identificando problemas e fornecendo soluções claras e práticas.

CÓDIGO PARA ANÁLISE:
```abap
{code}
"""
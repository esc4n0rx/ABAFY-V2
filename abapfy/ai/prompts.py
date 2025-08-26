# abapfy/ai/prompts.py
class ABAPPrompts:
    """
    Gerencia uma coleção de prompts de alta qualidade para geração, análise e
    depuração de código ABAP. Cada prompt é cuidadosamente projetado para instruir
    um modelo de linguagem a seguir as melhores práticas do ABAP moderno (7.5+),
    com foco em clareza, segurança e manutenibilidade.
    """

    @staticmethod
    def get_program_prompt(description: str, program_type: str = "REPORT") -> str:
        """
        Gera um prompt para a criação de um programa ABAP completo.

        Este prompt instrui a IA a atuar como um engenheiro sênior, utilizando
        classes locais, sintaxe moderna, e seguindo diretrizes estritas de
        Clean Code.

        Args:
            description: A descrição funcional detalhada do programa.
            program_type: O tipo de programa ABAP (ex: 'REPORT', 'MODULE POOL').

        Returns:
            O prompt formatado e pronto para ser enviado ao modelo de IA.
        """
        return f"""
### CONTEXTO
Você é um Engenheiro de Software ABAP Sênior, especialista em Clean Code e ABAP Moderno (7.5+). Sua principal habilidade é traduzir requisitos de negócio em código ABAP robusto, eficiente e de fácil manutenção.

### TAREFA
Sua tarefa é gerar um programa ABAP completo e funcional do tipo '{program_type}', com base na descrição funcional fornecida e seguindo rigorosamente todos os requisitos técnicos listados abaixo.

### DESCRIÇÃO FUNCIONAL
{description}

### REQUISITOS OBRIGATÓRIOS
- **Estrutura com Classes Locais:** É proibido o uso de sub-rotinas (PERFORM). Toda a lógica de negócio, acesso a dados e manipulação de UI deve ser encapsulada em uma ou mais classes locais (LCL). O bloco de eventos (START-OF-SELECTION) deve apenas instanciar e executar a classe principal.
- **Sintaxe Moderna (ABAP 7.5+):** Utilize preferencialmente expressões inline `DATA(...)` e `FIELD-SYMBOL(...)`. Use operadores de construção como `VALUE`, `CONV`, `NEW`, `COND`, `SWITCH`, e sempre que possível, utilize expressões de tabela e string templates.
- **Tratamento de Exceções:** Implemente o tratamento de erros exclusivamente com exceções baseadas em classes (`TRY...CATCH...ENDTRY` com classes `CX_...`). O uso de `sy-subrc` para controle de fluxo de erro não é permitido.
- **Acesso a Dados (SQL):** Nos comandos `SELECT`, liste explicitamente todos os campos necessários. O uso de `SELECT *` é estritamente proibido. Utilize a sintaxe SQL moderna com variáveis escapadas por `@` (ex: `WHERE carrid = @lv_carrid`).
- **Segurança:** Inclua verificações de autorização (`AUTHORITY-CHECK`) onde o acesso a dados sensíveis ou transações críticas for implícito na descrição.
- **Convenções de Nomenclatura SAP:** Siga as convenções de nomenclatura padrão (ex: `GT_` para tabelas globais, `LCL_` para classes locais, `LO_` para instâncias de objetos, `IV_` para parâmetros `IMPORTING`, `ES_` para `EXPORTING`, `CS_` para `CHANGING`, `RT_`/`RS_` para `RETURNING`).
- **Comentários:** O código deve ser comentado em português, explicando a finalidade de algoritmos complexos ou decisões de design não óbvias.

### ESTRUTURA DE CÓDIGO ESPERADA
1.  Cabeçalho do programa (autor, data, descrição).
2.  Declarações de tipos e dados globais (se estritamente necessários).
3.  Tela de Seleção (`PARAMETERS`, `SELECT-OPTIONS`), se a descrição exigir.
4.  Definição da Classe Local (`CLASS lcl_... DEFINITION`).
5.  Implementação da Classe Local (`CLASS lcl_... IMPLEMENTATION`).
6.  Bloco de Eventos (`INITIALIZATION`, `AT SELECTION-SCREEN`, `START-OF-SELECTION`) para orquestrar a execução da classe.

### REGRAS DE FORMATAÇÃO DA RESPOSTA
- Forneça APENAS o código ABAP puro.
- Não inclua explicações, comentários externos, ou qualquer texto antes ou depois do bloco de código.
- O código deve ser completo e pronto para ser copiado e ativado em um ambiente SAP.
"""

    @staticmethod
    def get_module_prompt(description: str, module_type: str) -> str:
        """
        Gera um prompt para a criação de um módulo ABAP (Método, Função, etc.).

        Foca na criação de módulos coesos, com assinaturas claras e tratamento
        de exceções moderno, seguindo o princípio da responsabilidade única.

        Args:
            description: A descrição funcional do módulo.
            module_type: O tipo de módulo (ex: 'MÉTODO DE CLASSE', 'MÓDULO DE FUNÇÃO').

        Returns:
            O prompt formatado para a geração do snippet de código.
        """
        return f"""
### CONTEXTO
Você é um Arquiteto de Software ABAP, especialista em design de APIs, modularização e código reutilizável.

### TAREFA
Crie um snippet de código para um(a) '{module_type}' ABAP, seguindo a descrição funcional e as diretrizes técnicas obrigatórias.

### DESCRIÇÃO FUNCIONAL
{description}

### DIRETRIZES TÉCNICAS OBRIGATÓRIAS
- **Assinatura Clara e Eficiente:** Para MÉTODOS com um único parâmetro de saída, utilize `RETURNING` ao invés de `EXPORTING` para permitir chamadas de método em expressões.
- **Princípio Fail-Fast:** Valide todos os parâmetros de importação no início do módulo para garantir que não são iniciais ou inválidos, lançando uma exceção imediatamente se a validação falhar.
- **Tratamento de Exceções Moderno:** Implemente o tratamento de erros usando exclusivamente exceções baseadas em classes (`RAISE EXCEPTION TYPE cx_...`). Não utilize `sy-subrc` ou flags de erro como parâmetros de saída.
- **Sintaxe Moderna (ABAP 7.4+):** Empregue expressões e operadores modernos (`VALUE`, `CONV`, `COND`, etc.) para um código mais conciso e legível.
- **Acesso a Dados Otimizado:** Se houver acesso ao banco de dados, liste os campos explicitamente no `SELECT`. `SELECT *` é proibido.
- **Responsabilidade Única:** O módulo deve ter uma única e bem definida responsabilidade.
- **Documentação (Cabeçalho):** Documente o propósito do módulo, todos os parâmetros (`IMPORTING`, `EXPORTING`, `CHANGING`, `RETURNING`) e as exceções que podem ser levantadas.

### REGRAS DE FORMATAÇÃO DA RESPOSTA
- Forneça APENAS o snippet de código ABAP para o(a) '{module_type}'.
- O código deve estar pronto para ser inserido em seu objeto correspondente (classe, grupo de funções, etc.).
- Não inclua texto explicativo, markdown ou qualquer conteúdo adicional.
"""

    @staticmethod
    def get_debug_prompt(code: str) -> str:
        """
        Gera um prompt para depuração e análise de causa raiz em código ABAP.

        Instrui a IA a atuar como um especialista em troubleshooting, focando em
        identificar erros, problemas de performance e segurança, e a propor
        soluções concretas.

        Args:
            code: O trecho de código ABAP a ser analisado.

        Returns:
            O prompt de depuração formatado.
        """
        return f"""
### CONTEXTO
Você é um Especialista Sênior em troubleshooting de código ABAP, com vasta experiência em depuração, otimização de performance e modernização de código legado.

### TAREFA
Analise rigorosamente o código ABAP fornecido. Identifique todos os erros potenciais (runtime, lógicos, sintáticos), vulnerabilidades de segurança e gargalos de performance. Forneça um relatório de análise detalhado e acionável, seguindo o formato de resposta obrigatório.

### CÓDIGO PARA ANÁLISE
```abap
{code}
```

### CRITÉRIOS DE ANÁLISE
- **Erros de Runtime:** Divisão por zero, acesso inválido a tabelas/ranges, referências de objeto nulas (`MOVE-CORRESPONDING` para objeto não instanciado), conversões de tipo inválidas.
- **Erros Lógicos:** Loops infinitos, condições `IF`/`CASE` incorretas, lógica de negócio falha, inicialização incorreta de variáveis.
- **Problemas de Performance:** `SELECT`s ineficientes (sem `WHERE` ou `JOIN`s ruins), `LOOP AT ... WHERE` em tabelas grandes, `SELECT` dentro de `LOOP`, processamento ineficiente de tabelas internas.
- **Vulnerabilidades de Segurança:** Falta de `AUTHORITY-CHECK`, exposição de dados sensíveis, falta de validação de entradas da tela de seleção.
- **Práticas Obsoletas:** Uso de `PERFORM`, `CALL TRANSACTION` sem `AUTHORITY-CHECK`, `SELECT *`, `sy-subrc` para controle de fluxo.

### FORMATO DE RESPOSTA OBRIGATÓRIO

**🐛 Problemas Identificados**
Liste de forma clara e objetiva cada problema encontrado, priorizando por severidade (Crítico, Alto, Médio, Baixo) e indicando a linha ou bloco de código.

**🔍 Análise Detalhada**
Para cada problema, explique a causa raiz e o impacto potencial (ex: "Risco de short dump `CX_SY_ZERODIVIDE`", "Leitura ineficiente que pode causar timeouts").

**🛠️ Soluções Propostas**
Forneça o código corrigido ou sugestões de implementação específicas para cada problema. As soluções devem ser práticas e usar ABAP moderno.

**⚡ Oportunidades de Modernização**
Sugira melhorias que vão além da correção, como refatorar um `LOOP` para uma expressão de tabela ou substituir lógica complexa por operadores como `COND` ou `SWITCH`.

**📋 Recomendações Adicionais**
Se aplicável, sugira o uso de ferramentas SAP (ex: SAT para análise de performance, Code Inspector para checagens estáticas) para uma validação mais profunda.
"""

    @staticmethod
    def get_review_prompt(code: str) -> str:
        """
        Gera um prompt detalhado para um Code Review estruturado.

        O prompt guia a IA para realizar uma análise 360°, cobrindo desde a
        qualidade e manutenibilidade até performance e segurança, e a apresentar
        o feedback de forma construtiva.

        Args:
            code: O trecho de código ABAP para o review.

        Returns:
            O prompt de code review formatado.
        """
        return f"""
### CONTEXTO
Você é um Arquiteto de Software ABAP Sênior, especialista em Code Review e aplicação dos princípios de Clean Code para garantir a qualidade e a longevidade do software.

### TAREFA
Analise rigorosamente o código ABAP fornecido. Forneça um code review detalhado, construtivo e educativo, focado em ajudar o desenvolvedor a melhorar a qualidade do seu código. Siga estritamente o formato de resposta abaixo.

### CÓDIGO PARA REVIEW
```abap
{code}
```

### CRITÉRIOS DE AVALIAÇÃO
- **Clean Code:** Legibilidade, nomenclatura clara, responsabilidade única (métodos e classes fazem uma só coisa), baixo acoplamento e alta coesão.
- **Sintaxe Moderna (ABAP 7.4+):** Uso de expressões inline, operadores `VALUE`/`CONV`/`COND`, string templates, expressões de tabela, etc.
- **Performance:** `SELECT`s otimizados, uso eficiente de tabelas internas (chaves, `LOOP AT REFERENCE`), evitação de operações custosas em loops.
- **Segurança:** Validação de entradas, tratamento robusto de exceções (`TRY/CATCH`), verificações de autorização (`AUTHORITY-CHECK`).
- **Manutenibilidade e Testabilidade:** Modularização adequada, documentação clara, ausência de "números mágicos" ou literais "hard-coded", código facilmente testável unitariamente.
- **Aderência aos Padrões SAP:** Convenções de nomenclatura, estrutura de programa, uso correto de APIs e BAPIs padrão.

### FORMATO DE RESPOSTA OBRIGATÓRIO

**✅ Pontos Positivos**
Liste os aspectos bem implementados no código. Reconheça o uso de boas práticas e padrões de design.

**⚠️ Pontos de Melhoria (Sugestões)**
Apresente sugestões para melhorar a legibilidade, performance ou manutenibilidade que não são críticas, mas que elevam a qualidade do código.

**❌ Problemas Críticos (Correção Obrigatória)**
Liste os problemas que devem ser corrigidos, como bugs, falhas de segurança, ou violações graves de boas práticas que comprometem a funcionalidade ou estabilidade da aplicação.

**🚀 Sugestões de Refatoração**
Para os pontos mais importantes, forneça exemplos de código "antes" e "depois", demonstrando como aplicar a modernização ou um padrão de design sugerido.

**📊 Resumo da Qualidade do Código**
- **Complexidade Ciclomática (Estimada):** [Baixa/Média/Alta]
- **Tratamento de Erros:** [Robusto/Parcial/Inexistente]
- **Aderência ao ABAP Moderno:** [Alta/Média/Baixa]

**💡 Recomendações Gerais**
Ofereça conselhos finais para o desenvolvedor, como focar em testes unitários para uma determinada classe ou estudar um conceito específico de ABAP moderno.
"""
    
    @staticmethod
    def get_embedding_analysis_prompt(chunks_summary: str, analysis_type: str) -> str:
        """
        Gera prompt para consolidar a análise de múltiplos chunks de código.

        Usado em um fluxo de análise de código extenso, onde o código foi dividido,
        analisado em partes (chunks), e os resultados precisam ser consolidados
        com uma visão arquitetural.

        Args:
            chunks_summary: Um resumo dos achados nos chunks individuais.
            analysis_type: O tipo de análise ('Code Review' ou 'Debug').

        Returns:
            O prompt de consolidação formatado.
        """
        return f"""
### CONTEXTO
Você é um Especialista Sênior em Arquitetura de Software ABAP. Sua tarefa é realizar uma análise consolidada de um código-fonte extenso que foi previamente dividido em chunks semânticos e analisado individualmente.

### TIPO DE ANÁLISE
{analysis_type.upper()}

### INSUMOS PARA ANÁLISE
Você recebeu um resumo dos problemas e observações encontrados nos chunks mais relevantes, identificados por uma análise de embeddings.
```text
{chunks_summary}
```

### TAREFA PRINCIPAL
Sua tarefa é ir além da análise individual dos chunks e fornecer uma visão holística e arquitetural. Consolide os achados, identifique padrões recorrentes e forneça recomendações estruturais que afetam o código como um todo.

### OBJETIVOS DA ANÁLISE
- **Consolidar Achados:** Unifique problemas similares encontrados em diferentes chunks em uma única observação (ex: "O padrão de não tratar exceções de `SELECT` se repete nos módulos de vendas e finanças").
- **Identificar Problemas Arquiteturais:** Analise como os problemas em diferentes chunks se relacionam. Existe um alto acoplamento problemático? Uma classe "Deus" centralizando responsabilidades?
- **Priorizar Correções:** Classifique os problemas consolidados por severidade e impacto no sistema como um todo.
- **Propor Melhorias Estruturais:** Sugira refatorações que beneficiem toda a base de código, como a introdução de uma classe de factory, um serviço de log centralizado, ou a refatoração de um modelo de dados.

### FORMATO DE RESPOSTA OBRIGATÓRIO

**🎯 Análise Consolidada Geral**
Um resumo executivo dos padrões de problemas (positivos e negativos) identificados em toda a base de código analisada.

**🔥 Problemas Arquiteturais Críticos**
Liste os 1-3 problemas de design ou estrutura mais graves que afetam múltiplos componentes e representam o maior risco para a manutenibilidade e estabilidade do sistema.

**💡 Recomendações Estruturais de Melhoria**
Para cada problema crítico, proponha uma solução de alto nível. Descreva o padrão de design recomendado ou a abordagem de refatoração que deveria ser adotada.

**📋 Roadmap de Correções Sugerido**
Proponha uma ordem lógica para atacar os problemas, começando pelos que destravam outras melhorias ou mitigam os maiores riscos.
"""

    @staticmethod
    def get_chunk_context_prompt(chunk_content: str, chunk_info: str, analysis_type: str) -> str:
        """
        Gera um prompt para a análise focada de um único chunk de código.

        Utilizado na primeira fase de uma análise de código extenso, onde um
        chunk específico, identificado como potencialmente problemático (ex: por
        embeddings), é analisado em detalhe.

        Args:
            chunk_content: O conteúdo de código do chunk.
            chunk_info: Metadados sobre o chunk (ex: origem, nome do objeto).
            analysis_type: O tipo de análise a ser feita ('Code Review' ou 'Debug').

        Returns:
            O prompt de análise de chunk formatado.
        """
        return f"""
### CONTEXTO
Você é um Especialista de Software ABAP realizando uma análise focada em um trecho específico (chunk) de código. Este chunk foi pré-selecionado por uma análise automatizada como sendo de alta relevância ou potencialmente problemático.

### TIPO DE ANÁLISE
{analysis_type.upper()}

### INFORMAÇÕES DO CHUNK
{chunk_info}

### CÓDIGO DO CHUNK PARA ANÁLISE
```abap
{chunk_content}
```

### TAREFA
Realize uma análise detalhada e técnica **apenas deste chunk de código**. Ignore o contexto externo e foque exclusivamente nos problemas, riscos e oportunidades de melhoria contidos neste fragmento.

### OBJETIVOS ESPECÍFICOS
- **Se for Code Review:** Identifique violações de Clean Code, uso de sintaxe obsoleta, e potenciais problemas de manutenibilidade ou performance dentro deste chunk.
- **Se for Debug:** Identifique a causa raiz mais provável de um bug, possíveis `short dumps`, ou fluxos lógicos incorretos dentro deste chunk.

### FORMATO DE RESPOSTA OBRIGATÓRIO

**🎯 Análise do Chunk**
Resumo dos 2-3 problemas mais significativos identificados neste fragmento e sua severidade.

**🔍 Detalhamento Técnico**
Para cada problema, aponte a linha específica e explique tecnicamente por que é um problema.

**🛠️ Correções Propostas**
Forneça o código corrigido e otimizado para os problemas identificados, aplicando as melhores práticas de ABAP moderno.

**⚡ Otimizações Adicionais (Opcional)**
Se houver, sugira melhorias de performance ou legibilidade que podem ser aplicadas a este chunk.
"""
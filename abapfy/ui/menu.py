import os
from pathlib import Path
import click
from typing import Any, Dict, Callable
from abapfy.config.manager import ConfigManager
from abapfy.ui.colors import print_colored, colorize
from abapfy.generators.program import ProgramGenerator
from abapfy.generators.module import ModuleGenerator
from abapfy.ai.client import AIClient
from abapfy.utils.loading import LoadingContext, ProgressSpinner

class MainMenu:
    """Menu principal do ABAPFY"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
        self.ai_client = AIClient(config)
        self.program_gen = ProgramGenerator(self.ai_client, config)
        self.module_gen = ModuleGenerator(self.ai_client, config)
        
        self.options: Dict[str, Callable] = {
            "1": self.generate_program,
            "2": self.generate_module,
            "3": self.debug_code,
            "4": self.code_review,
            "5": self.show_settings,
            "6": self.exit_app
        }
    
    def run(self):
        """Executa o loop principal do menu"""
        user_name = self.config.get("user_name", "Dev")
        color = self.config.get("text_color", "cyan")
        
        while True:
            self._show_menu(user_name, color)
            choice = self._get_user_choice()
            
            if choice in self.options:
                try:
                    self.options[choice]()
                except KeyboardInterrupt:
                    print_colored("\n⏸️  Operação cancelada.", "yellow")
                except Exception as e:
                    print_colored(f"❌ Erro: {str(e)}", "red")
                
                if choice != "6":  # Não pausar se for sair
                    input(colorize("\nPressione Enter para continuar...", "yellow"))
            else:
                print_colored("❌ Opção inválida! Tente novamente.", "red")
                input(colorize("Pressione Enter para continuar...", "yellow"))
    
    def _show_menu(self, user_name: str, color: str):
        """Exibe o menu principal"""
        print_colored(f"\n👋 Olá, {user_name}! O que você gostaria de fazer hoje?\n", color, bold=True)
        
        menu_items = [
            ("1", "🚀 Gerar Programa ABAP", "Criar um novo programa ABAP completo"),
            ("2", "📦 Gerar Módulo", "Criar módulos, funções ou métodos"),
            ("3", "🔍 Debug de Código", "Analisar e corrigir problemas no código"),
            ("4", "📋 Code Review", "Revisar e melhorar código existente"),
            ("5", "⚙️  Configurações", "Ajustar preferências do sistema"),
            ("6", "🚪 Sair", "Encerrar o ABAPFY")
        ]
        
        for option, title, description in menu_items:
            print_colored(f"  {option}. ", "white", end="")
            print_colored(title, color, bold=True, end="")
            print_colored(f" - {description}", "white")
        
        print()
    
    def _get_user_choice(self) -> str:
        """Obtém escolha do usuário"""
        return click.prompt(
            colorize("Escolha uma opção", self.config.get("text_color", "cyan")),
            type=str
        ).strip()
    
    def generate_program(self):
        """Gera um programa ABAP"""
        if not self.ai_client.is_configured():
            print_colored("❌ API não configurada! Configure a variável OPENAI_API_KEY.", "red")
            return
        
        print_colored("🚀 Gerador de Programa ABAP", "cyan", bold=True)
        print_colored("=" * 50, "cyan")
        
        self.program_gen.generate()
    
    def generate_module(self):
        """Gera um módulo ABAP"""
        if not self.ai_client.is_configured():
            print_colored("❌ API não configurada! Configure a variável OPENAI_API_KEY.", "red")
            return
        
        print_colored("📦 Gerador de Módulo ABAP", "cyan", bold=True)
        print_colored("=" * 50, "cyan")
        
        self.module_gen.generate()
    
    def debug_code(self):
        """Debug de código ABAP com embeddings e loading melhorado"""
        if not self.ai_client.is_configured():
            print_colored("❌ API não configurada! Configure a variável OPENAI_API_KEY.", "red")
            return
        
        from abapfy.utils.file_manager import ABAPFileManager
        from abapfy.analyzers.debugger import ABAPDebugger
        
        print_colored("🔍 Debug de Código ABAP", "cyan", bold=True)
        print_colored("=" * 50, "cyan")
        
        # Inicializar gerenciadores
        file_manager = ABAPFileManager()
        debugger = ABAPDebugger(self.config)
        
        # Criar estrutura de pastas se necessário
        file_manager.create_codes_directory_structure()
        
        # Listar arquivos disponíveis
        files = file_manager.list_abap_files()
        selected_file = file_manager.display_files_menu(files)
        
        if not selected_file:
            return
        
        # Validar arquivo
        is_valid, message = file_manager.validate_abap_file(selected_file)
        if not is_valid:
            print_colored(f"❌ {message}", "red")
            return
        
        print_colored(f"📁 Arquivo selecionado: {Path(selected_file).name}", "green")
        
        # Obter informações sobre o erro com interface melhorada
        print_colored("\n" + "="*60, "cyan")
        print_colored("🤔 INFORMAÇÕES SOBRE O PROBLEMA", "cyan", bold=True)
        print_colored("="*60, "cyan")
        
        print_colored("Descreva o problema/erro encontrado:", "yellow")
        print_colored("(Ex: 'Dump ZERODIVIDE na linha 150' ou 'Loop infinito no método XYZ')", "white")
        print_colored("Deixe vazio se quiser uma análise geral ➜", "cyan", end=" ")
        error_description = input().strip()
        
        error_location = None
        if error_description:
            print_colored("\nEm que linha ou método ocorre o problema?", "yellow")
            print_colored("(Ex: 'linha 150', 'método GET_DATA', 'FORM validate_input')", "white") 
            print_colored("Deixe vazio se não souber ➜", "cyan", end=" ")
            error_location = input().strip() or None
        
        try:
            print_colored("\n" + "="*60, "green")
            print_colored("🤖 INICIANDO ANÁLISE DE DEBUG", "green", bold=True)
            print_colored("="*60, "green")
            
            # Executar análise de debug com loading
            debug_report = debugger.analyze_code_for_debug(
                selected_file, error_description, error_location
            )
            
            # Exibir relatório
            self._display_debug_report(debug_report)
            
            # Oferecer para salvar relatório
            self._save_debug_report(debug_report)
            
        except Exception as e:
            print_colored(f"❌ Erro durante debug: {str(e)}", "red")
    
    def code_review(self):
        """Code review de código ABAP com embeddings e loading melhorado"""
        if not self.ai_client.is_configured():
            print_colored("❌ API não configurada! Configure a variável OPENAI_API_KEY.", "red")
            return
        
        from abapfy.utils.file_manager import ABAPFileManager
        from abapfy.analyzers.code_reviewer import CodeReviewer
        
        print_colored("📋 Code Review ABAP", "cyan", bold=True)
        print_colored("=" * 50, "cyan")
        
        # Inicializar gerenciadores
        file_manager = ABAPFileManager()
        reviewer = CodeReviewer(self.config)
        
        # Criar estrutura de pastas se necessário
        file_manager.create_codes_directory_structure()
        
        # Listar arquivos disponíveis
        files = file_manager.list_abap_files()
        selected_file = file_manager.display_files_menu(files)
        
        if not selected_file:
            return
        
        # Validar arquivo
        is_valid, message = file_manager.validate_abap_file(selected_file)
        if not is_valid:
            print_colored(f"❌ {message}", "red")
            return
        
        print_colored(f"📁 Arquivo selecionado: {Path(selected_file).name}", "green")
        
        try:
            print_colored("\n" + "="*60, "green")
            print_colored("📋 INICIANDO CODE REVIEW", "green", bold=True)
            print_colored("="*60, "green")
            
            # Executar code review com loading
            review_report = reviewer.analyze_code(selected_file)
            
            # Exibir relatório
            self._display_review_report(review_report)
            
            # Oferecer para salvar relatório
            self._save_review_report(review_report)
            
        except Exception as e:
            print_colored(f"❌ Erro durante code review: {str(e)}", "red")
    
    def _display_debug_report(self, report: Dict[str, Any]):
        """Exibe relatório de debug formatado com melhorias"""
        print_colored("\n" + "="*70, "green")
        print_colored("🔍 RELATÓRIO DE DEBUG ABAP", "green", bold=True)
        print_colored("="*70, "green")
        
        # Summary melhorado
        summary = report['debug_summary']
        print_colored(f"\n📊 RESUMO DA ANÁLISE:", "cyan", bold=True)
        print_colored(f"   📋 Arquivo: {Path(report['metadata']['file_path']).name}", "white")
        print_colored(f"   📐 Total de linhas: {report['metadata']['total_lines']}", "white")
        print_colored(f"   🧩 Chunks analisados: {report['metadata']['total_chunks']}", "white")
        print_colored(f"   🚨 Prioridade: {summary['debug_priority']}", "white")
        print_colored(f"   🎯 Confiança: {summary['confidence_score']}%", "white")
        print_colored(f"   📈 Issues encontrados: {summary['total_issues_found']}", "white")
        print_colored(f"   🔍 Causa provável: {summary['most_likely_cause']}", "white")
        
        # Mostrar problemas críticos se existirem
        if summary.get('critical_issues', 0) > 0:
            print_colored(f"\n🚨 PROBLEMAS CRÍTICOS DETECTADOS:", "red", bold=True)
            regex_analysis = report.get('regex_analysis', {})
            for pattern_name, issues in regex_analysis.items():
                if issues and any(issue.get('severity') == 'CRITICAL' for issue in issues):
                    print_colored(f"   ❌ {pattern_name.replace('_', ' ').title()}:", "red")
                    for issue in issues[:3]:  # Mostrar apenas primeiros 3
                        if issue.get('severity') == 'CRITICAL':
                            print_colored(f"      📍 Linha {issue['line']}: {issue['description']}", "white")
        
        # Problemas de alta prioridade
        if summary.get('high_issues', 0) > 0:
            print_colored(f"\n⚠️ PROBLEMAS DE ALTA PRIORIDADE:", "red")
            regex_analysis = report.get('regex_analysis', {})
            count = 0
            for pattern_name, issues in regex_analysis.items():
                if issues and any(issue.get('severity') == 'HIGH' for issue in issues) and count < 5:
                    for issue in issues:
                        if issue.get('severity') == 'HIGH' and count < 5:
                            print_colored(f"   🔸 Linha {issue['line']}: {issue['description']}", "white")
                            count += 1
        
        # Sugestões
        if report.get('debug_suggestions'):
            print_colored(f"\n💡 SUGESTÕES DE CORREÇÃO:", "yellow", bold=True)
            for i, suggestion in enumerate(report['debug_suggestions'][:5], 1):
                print_colored(f"   {i}. {suggestion}", "white")
        
        # Próximos passos
        if report.get('next_steps'):
            print_colored(f"\n📋 PRÓXIMOS PASSOS:", "magenta", bold=True)
            for step in report['next_steps']:
                print_colored(f"   {step}", "white")
        
        # Análise detalhada se existir
        if report.get('detailed_debug_analysis'):
            print_colored(f"\n🤖 ANÁLISE DETALHADA DA IA:", "blue", bold=True)
            for chunk_name, analysis in report['detailed_debug_analysis'].items():
                print_colored(f"   📄 {chunk_name}:", "cyan")
                # Mostrar apenas primeiras linhas para não poluir
                analysis_lines = analysis.split('\n')[:4]
                for line in analysis_lines:
                    if line.strip():
                        print_colored(f"      {line}", "white")
                if len(analysis.split('\n')) > 4:
                    print_colored(f"      ... (análise completa no relatório salvo)", "yellow")
    
    def _display_review_report(self, report: Dict[str, Any]):
        """Exibe relatório de code review formatado com melhorias"""
        print_colored("\n" + "="*70, "green")
        print_colored("📋 RELATÓRIO DE CODE REVIEW", "green", bold=True)
        print_colored("="*70, "green")
        
        # Summary
        summary = report['summary']
        print_colored(f"\n📊 AVALIAÇÃO GERAL:", "cyan", bold=True)
        print_colored(f"   📋 Arquivo: {Path(report['metadata']['file_path']).name}", "white")
        print_colored(f"   📐 Total de linhas: {report['metadata']['total_lines']}", "white")
        print_colored(f"   🧩 Chunks analisados: {summary['chunks_analyzed']}", "white")
        print_colored(f"   ⭐ Rating: {summary['overall_rating']}", "white")
        print_colored(f"   📊 Score: {summary['severity_score']}/100", "white")
        print_colored(f"   🔍 Issues encontrados: {summary['total_issues_found']}", "white")
        
        # Análise por categoria
        print_colored(f"\n🔍 ANÁLISE POR CATEGORIA:", "yellow", bold=True)
        for category, analysis in report['analysis_by_category'].items():
            issues = analysis['issues_found']
            if issues > 0:
                status = "🔴" if issues > 3 else "🟡" if issues > 1 else "🟢"
                category_name = category.replace('_', ' ').title()
                print_colored(f"   {status} {category_name}: {issues} problemas", "white")
        
        # Recomendações
        if report.get('recommendations'):
            print_colored(f"\n💡 RECOMENDAÇÕES PRINCIPAIS:", "magenta", bold=True)
            for i, rec in enumerate(report['recommendations'][:5], 1):
                print_colored(f"   {i}. {rec}", "white")
        
        # Análise detalhada resumida
        if report.get('detailed_findings'):
            print_colored(f"\n🤖 ANÁLISE DETALHADA DA IA:", "blue", bold=True)
            findings_count = len(report['detailed_findings'])
            if findings_count > 0:
                print_colored(f"   📄 {findings_count} chunk(s) analisado(s) em detalhes", "cyan")
                print_colored(f"   💾 Análise completa disponível no relatório salvo", "yellow")
    
    def _save_debug_report(self, report: Dict[str, Any]):
        """Salva relatório de debug"""
        if click.confirm(colorize("💾 Deseja salvar o relatório de debug?", "cyan")):
            try:
                with LoadingContext("Salvando relatório", "spinner"):
                    output_dir = Path("output/debug_reports")
                    output_dir.mkdir(parents=True, exist_ok=True)
                    
                    timestamp = report['metadata']['analysis_timestamp'].replace(':', '-').replace(' ', '_')
                    filename = f"debug_report_{timestamp}.json"
                    file_path = output_dir / filename
                    
                    import json
                    with open(file_path, 'w', encoding='utf-8') as f:
                        json.dump(report, f, indent=2, ensure_ascii=False)
                
                print_colored(f"✅ Relatório salvo em: {file_path}", "green")
                
            except Exception as e:
                print_colored(f"❌ Erro ao salvar: {str(e)}", "red")
    
    def _save_review_report(self, report: Dict[str, Any]):
        """Salva relatório de code review"""
        if click.confirm(colorize("💾 Deseja salvar o relatório de review?", "cyan")):
            try:
                with LoadingContext("Salvando relatório", "spinner"):
                    output_dir = Path("output/review_reports")
                    output_dir.mkdir(parents=True, exist_ok=True)
                    
                    timestamp = report['metadata']['analysis_timestamp'].replace(':', '-').replace(' ', '_')
                    filename = f"review_report_{timestamp}.json"
                    file_path = output_dir / filename
                    
                    import json
                    with open(file_path, 'w', encoding='utf-8') as f:
                        json.dump(report, f, indent=2, ensure_ascii=False)
                
                print_colored(f"✅ Relatório salvo em: {file_path}", "green")
                
            except Exception as e:
                print_colored(f"❌ Erro ao salvar: {str(e)}", "red")
    
    def show_settings(self):
        """Exibe menu de configurações"""
        settings_menu = SettingsMenu(self.config)
        settings_menu.run()
    
    def exit_app(self):
        """Sair da aplicação"""
        user_name = self.config.get("user_name", "Dev")
        print_colored(f"👋 Até logo, {user_name}! Obrigado por usar o ABAPFY!", "green", bold=True)
        exit(0)

class SettingsMenu:
    """Menu de configurações"""
    
    def __init__(self, config: ConfigManager):
        self.config = config
    
    def run(self):
        """Executa menu de configurações"""
        while True:
            self._show_settings_menu()
            choice = input(colorize("Escolha uma opção: ", "cyan")).strip()
            
            if choice == "1":
                self._change_name()
            elif choice == "2":
                self._change_color()
            elif choice == "3":
                self._show_api_status()
            elif choice == "4":
                self._reset_settings()
            elif choice == "5":
                break
            else:
                print_colored("❌ Opção inválida!", "red")
            
            input(colorize("\nPressione Enter para continuar...", "yellow"))
    
    def _show_settings_menu(self):
        """Exibe menu de configurações"""
        print_colored("\n⚙️  Configurações do ABAPFY", "cyan", bold=True)
        print_colored("=" * 50, "cyan")
        
        user_name = self.config.get("user_name", "Dev")
        color = self.config.get("text_color", "cyan")
        api_status = "✅ Configurada" if self.config.is_api_configured() else "❌ Não configurada"
        
        print_colored(f"👤 Nome atual: {user_name}", "white")
        print_colored(f"🎨 Cor atual: ", "white", end="")
        print_colored(color.title(), color)
        print_colored(f"🔑 API Status: {api_status}", "white")
        print()
        
        print_colored("1. 📝 Alterar nome", "yellow")
        print_colored("2. 🎨 Alterar cor do texto", "yellow")
        print_colored("3. 🔑 Status da API", "yellow")
        print_colored("4. 🔄 Resetar configurações", "yellow")
        print_colored("5. ↩️  Voltar ao menu principal", "yellow")
        print()
    
    def _change_name(self):
        """Altera nome do usuário"""
        current_name = self.config.get("user_name", "Dev")
        new_name = click.prompt(f"Nome atual: {current_name}. Novo nome", default=current_name)
        self.config.set("user_name", new_name)
        print_colored(f"✅ Nome alterado para: {new_name}", "green")
    
    def _change_color(self):
        """Altera cor do texto"""
        print_colored("Escolha a nova cor:", "cyan")
        colors = ["cyan", "green", "yellow", "blue", "magenta", "white", "red"]
        
        for i, color in enumerate(colors, 1):
            print_colored(f"  {i}. {color.title()}", color)
        
        try:
            choice = click.prompt("Escolha (1-7)", type=click.IntRange(1, 7))
            selected_color = colors[choice - 1]
            self.config.set("text_color", selected_color)
            print_colored(f"✅ Cor alterada para: ", "green", end="")
            print_colored(selected_color.title(), selected_color)
        except click.Abort:
            print_colored("❌ Operação cancelada.", "yellow")
    
    def _show_api_status(self):
        """Mostra status da API"""
        if self.config.is_api_configured():
            print_colored("✅ API configurada corretamente!", "green")
            print_colored("🔗 Base URL: " + str(os.getenv('OPENAI_BASE_URL', 'https://conductor.arcee.ai/v1')), "white")
        else:
            print_colored("❌ API não configurada!", "red")
            print_colored("Configure as variáveis de ambiente:", "yellow")
            print_colored("  OPENAI_API_KEY=seu_token_arcee", "white")
            print_colored("  OPENAI_BASE_URL=https://conductor.arcee.ai/v1", "white")
    
    def _reset_settings(self):
        """Reseta configurações"""
        if click.confirm(colorize("⚠️  Tem certeza que deseja resetar todas as configurações?", "red")):
            self.config.reset_to_defaults()
            print_colored("✅ Configurações resetadas!", "green")
        else:
            print_colored("❌ Operação cancelada.", "yellow")
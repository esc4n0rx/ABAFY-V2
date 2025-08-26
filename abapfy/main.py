#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import click
from abapfy.ui.banner import show_banner
from abapfy.ui.menu import MainMenu
from abapfy.config.manager import ConfigManager
from abapfy.ui.colors import print_colored

def main():
    """Ponto de entrada principal do ABAPFY"""
    try:
        # Inicializar configurações
        config = ConfigManager()
        
        # Mostrar banner
        show_banner()
        
        # Verificar configuração inicial
        if config.get("first_run", True):
            setup_first_run(config)
        
        # Iniciar menu principal
        menu = MainMenu(config)
        menu.run()
        
    except KeyboardInterrupt:
        print_colored("\n👋 Até logo!", "yellow")
        sys.exit(0)
    except Exception as e:
        print_colored(f"❌ Erro: {str(e)}", "red")
        sys.exit(1)

def setup_first_run(config: ConfigManager):
    """Configuração inicial do sistema"""
    print_colored("🎉 Bem-vindo ao ABAPFY!", "green", bold=True)
    print_colored("Vamos configurar sua experiência inicial.\n", "cyan")
    
    # Configurar nome do usuário
    user_name = click.prompt(
        click.style("Como você gostaria de ser chamado?", fg="cyan"),
        default="Dev",
        type=str
    )
    
    # Configurar cor do texto
    print_colored("\nEscolha a cor do texto:", "cyan")
    colors = ["cyan", "green", "yellow", "blue", "magenta", "white"]
    for i, color in enumerate(colors, 1):
        print_colored(f"  {i}. {color.title()}", color)
    
    color_choice = click.prompt(
        click.style("Escolha (1-6)", fg="cyan"),
        default=1,
        type=click.IntRange(1, 6)
    )
    
    selected_color = colors[color_choice - 1]
    
    # Salvar configurações
    config.set("user_name", user_name)
    config.set("text_color", selected_color)
    config.set("first_run", False)
    
    print_colored(f"\n✅ Configuração concluída! Olá, {user_name}!", "green")
    print_colored("Pressione Enter para continuar...", "yellow")
    input()

if __name__ == "__main__":
    main()
# abapfy/utils/loading.py
import sys
import time
import threading
from typing import Optional, List
from abapfy.ui.colors import print_colored, colorize

class ProgressSpinner:
    """Spinner animado para operações longas"""
    
    def __init__(self, message: str = "Processando", color: str = "cyan"):
        self.message = message
        self.color = color
        self.running = False
        self.thread: Optional[threading.Thread] = None
        self.spinner_chars = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
        self.current_frame = 0
    
    def start(self):
        """Inicia o spinner"""
        self.running = True
        self.thread = threading.Thread(target=self._animate, daemon=True)
        self.thread.start()
    
    def stop(self, success_message: Optional[str] = None):
        """Para o spinner"""
        if self.running:
            self.running = False
            if self.thread:
                self.thread.join()
            
            # Limpar linha
            sys.stdout.write('\r' + ' ' * 80 + '\r')
            sys.stdout.flush()
            
            if success_message:
                print_colored(f"✅ {success_message}", "green")
    
    def _animate(self):
        """Animação do spinner"""
        while self.running:
            char = self.spinner_chars[self.current_frame]
            sys.stdout.write(f'\r{colorize(char + " " + self.message, self.color)}')
            sys.stdout.flush()
            self.current_frame = (self.current_frame + 1) % len(self.spinner_chars)
            time.sleep(0.1)

class ProgressBar:
    """Barra de progresso simples"""
    
    def __init__(self, total: int, message: str = "Progresso", width: int = 40, color: str = "green"):
        self.total = total
        self.current = 0
        self.message = message
        self.width = width
        self.color = color
    
    def update(self, amount: int = 1, status: str = ""):
        """Atualiza o progresso"""
        self.current = min(self.current + amount, self.total)
        self._render(status)
    
    def set_progress(self, value: int, status: str = ""):
        """Define progresso absoluto"""
        self.current = min(value, self.total)
        self._render(status)
    
    def finish(self, message: str = "Concluído"):
        """Finaliza a barra"""
        self.current = self.total
        self._render()
        print_colored(f"\n✅ {message}", "green")
    
    def _render(self, status: str = ""):
        """Renderiza a barra"""
        if self.total == 0:
            percent = 100
            filled = self.width
        else:
            percent = (self.current / self.total) * 100
            filled = int((self.current / self.total) * self.width)
        
        bar = "█" * filled + "░" * (self.width - filled)
        
        status_text = f" - {status}" if status else ""
        progress_text = f"{self.message}: {percent:5.1f}% [{bar}] {self.current}/{self.total}{status_text}"
        
        sys.stdout.write(f'\r{colorize(progress_text, self.color)}')
        sys.stdout.flush()

class LoadingContext:
    """Context manager para loading com diferentes tipos"""
    
    def __init__(self, message: str, loading_type: str = "spinner", 
                 total: Optional[int] = None, color: str = "cyan"):
        self.message = message
        self.loading_type = loading_type
        self.total = total
        self.color = color
        self.loader = None
    
    def __enter__(self):
        if self.loading_type == "spinner":
            self.loader = ProgressSpinner(self.message, self.color)
            self.loader.start()
        elif self.loading_type == "bar" and self.total:
            self.loader = ProgressBar(self.total, self.message, color=self.color)
        
        return self.loader
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.loader:
            if hasattr(self.loader, 'stop'):
                if exc_type is None:
                    self.loader.stop("Concluído")
                else:
                    self.loader.stop()
            elif hasattr(self.loader, 'finish'):
                if exc_type is None:
                    self.loader.finish()

def show_step_progress(steps: List[str], color: str = "cyan"):
    """Mostra progresso por etapas"""
    total_steps = len(steps)
    
    for i, step in enumerate(steps, 1):
        print_colored(f"📋 Etapa {i}/{total_steps}: {step}", color)
        
        # Simular trabalho
        with ProgressSpinner(f"Executando etapa {i}", color):
            yield i, step
        
        print_colored(f"✅ Etapa {i} concluída", "green")
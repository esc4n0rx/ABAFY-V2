import json
import os
from pathlib import Path
from typing import Any, Dict, Optional
from dotenv import load_dotenv

load_dotenv()

class ConfigManager:
    """Gerenciador de configurações do ABAPFY"""
    
    DEFAULT_CONFIG = {
        "user_name": "Dev",
        "text_color": "cyan",
        "model": "auto",
        "temperature": 0.4,
        "first_run": True,
        "api_configured": False
    }
    
    def __init__(self):
        self.config_dir = Path.home() / ".abapfy"
        self.config_file = self.config_dir / "config.json"
        self.config = self._load_config()
    
    def _load_config(self) -> Dict[str, Any]:
        """Carrega configurações do arquivo"""
        if not self.config_file.exists():
            return self.DEFAULT_CONFIG.copy()
        
        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                config = json.load(f)
                # Mesclar com configurações padrão para garantir chaves necessárias
                merged_config = self.DEFAULT_CONFIG.copy()
                merged_config.update(config)
                return merged_config
        except Exception:
            return self.DEFAULT_CONFIG.copy()
    
    def save_config(self):
        """Salva configurações no arquivo"""
        self.config_dir.mkdir(exist_ok=True)
        try:
            with open(self.config_file, 'w', encoding='utf-8') as f:
                json.dump(self.config, f, indent=2, ensure_ascii=False)
        except Exception as e:
            raise RuntimeError(f"Erro ao salvar configurações: {e}")
    
    def get(self, key: str, default: Any = None) -> Any:
        """Obtém valor de configuração"""
        return self.config.get(key, default)
    
    def set(self, key: str, value: Any):
        """Define valor de configuração"""
        self.config[key] = value
        self.save_config()
    
    def update(self, updates: Dict[str, Any]):
        """Atualiza múltiplas configurações"""
        self.config.update(updates)
        self.save_config()
    
    def reset_to_defaults(self):
        """Reseta configurações para padrões"""
        self.config = self.DEFAULT_CONFIG.copy()
        self.save_config()
    
    def is_api_configured(self) -> bool:
        """Verifica se a API está configurada"""
        api_key = os.getenv('OPENAI_API_KEY') or os.getenv('ARCEE_TOKEN')
        return bool(api_key)
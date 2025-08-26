from setuptools import setup, find_packages

setup(
    name="abapfy",
    version="1.0.0",
    description="CLI Tool para Desenvolvimento ABAP usando IA",
    author="Dev Team",
    packages=find_packages(),
    install_requires=[
        "click>=8.0.0",
        "openai>=1.0.0",
        "colorama>=0.4.6",
        "rich>=13.0.0",
        "pydantic>=2.0.0"
    ],
    entry_points={
        'console_scripts': [
            'abapfy=abapfy.main:main',
        ],
    },
    python_requires=">=3.8",
)
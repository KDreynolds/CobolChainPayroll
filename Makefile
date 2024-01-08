# Makefile for COBOL-Python Blockchain Payroll Application

# Variables
COBOL_COMPILER = cobc
PYTHON = python3
COBOL_FLAGS = -x -free
PYTHON_FILES = blockchain.py transaction.py block.py chain_utils.py blockchain_interface.py
COBOL_FILES = payroll.cbl integration.cbl
COPYBOOKS = employee_record.cpy payroll_record.cpy payroll_config.cpy

# Default make target
all: payroll blockchain

# Compile COBOL files
payroll: $(COBOL_FILES) $(COPYBOOKS)
	$(COBOL_COMPILER) $(COBOL_FLAGS) -o payroll $(COBOL_FILES)

# Run Python blockchain
blockchain: $(PYTHON_FILES)
	$(PYTHON) -m blockchain

# Clean up the compiled files
clean:
	rm -f payroll
	find . -name '*.pyc' -delete
	find . -name '__pycache__' -delete

# Install Python dependencies
install:
	pip install -r requirements.txt

# Help command to display makefile targets
help:
	@echo "Available make targets:"
	@echo "  all       - Compile COBOL files and run Python blockchain"
	@echo "  payroll   - Compile COBOL payroll application"
	@echo "  blockchain - Run Python blockchain"
	@echo "  clean     - Clean up compiled files"
	@echo "  install   - Install Python dependencies"
	@echo "  help      - Display this help message"

.PHONY: all payroll blockchain clean install help

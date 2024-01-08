# COBOL Payroll and Python Blockchain Integration

This project is a COBOL-based payroll application that integrates with a Python-implemented blockchain. The purpose of this integration is to provide an immutable record of payroll transactions, ensuring easier auditing and compliance with financial regulations.

## Features

- Payroll processing using COBOL
- Immutable transaction records using a Python blockchain
- Consensus algorithm for resolving conflicts in the blockchain network
- Easy node registration for blockchain network expansion

## File Structure

- `blockchain.py`: The main blockchain implementation in Python.
- `transaction.py`: Defines the structure and behavior of a transaction.
- `block.py`: Defines the structure and behavior of a block.
- `chain_utils.py`: Utility functions for the blockchain, such as hashing and validation.
- `blockchain_config.json`: Configuration settings for the blockchain.
- `payroll.cbl`: The COBOL program for processing payroll.
- `employee_record.cpy`: The COBOL copybook for employee records.
- `payroll_record.cpy`: The COBOL copybook for payroll records.
- `payroll_config.cpy`: The COBOL copybook for payroll configuration.
- `integration.cbl`: The COBOL program for integrating with the Python blockchain.
- `blockchain_interface.py`: The Python interface for interacting with the COBOL application.
- `requirements.txt`: The Python dependencies required for the blockchain.
- `README.md`: This file, describing the project and how to use it.
- `Makefile`: A makefile to automate compilation and other tasks.

## Prerequisites

- Python 3.x
- COBOL compiler (e.g., GnuCOBOL)
- Python libraries as listed in `requirements.txt`

## Setup

1. Install the required Python libraries:

```bash
pip install -r requirements.txt
```

2. Compile the COBOL programs:

```bash
make compile
```

## Running the Application

1. Start the blockchain node:

```bash
python blockchain.py
```

2. Run the COBOL payroll application:

```bash
cobcrun payroll
```

## Contributing

Contributions to this project are welcome. Please ensure that any pull requests or issues are consistent with the project's goals and coding standards.

## License

This project is open source and available under the [MIT License](LICENSE).

## Disclaimer

This project is for demonstration purposes only and is not intended for use in production environments.

## Contact

For any inquiries or contributions, please contact the repository owner.


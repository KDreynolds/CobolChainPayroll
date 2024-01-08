import requests
from blockchain import Blockchain

class BlockchainInterface:
    def __init__(self, blockchain_host, blockchain_port):
        self.blockchain_url = f"http://{blockchain_host}:{blockchain_port}"

    def get_full_chain(self):
        response = requests.get(f"{self.blockchain_url}/chain")
        if response.status_code == 200:
            return response.json()
        return None

    def mine_block(self):
        response = requests.get(f"{self.blockchain_url}/mine")
        if response.status_code == 200:
            return response.json()
        return None

    def new_transaction(self, sender, recipient, amount):
        transaction_data = {
            'sender': sender,
            'recipient': recipient,
            'amount': amount
        }
        response = requests.post(f"{self.blockchain_url}/transactions/new", json=transaction_data)
        if response.status_code == 201:
            return response.json()
        return None

    def register_node(self, node_url):
        node_data = {
            'nodes': [node_url]
        }
        response = requests.post(f"{self.blockchain_url}/nodes/register", json=node_data)
        if response.status_code == 201:
            return response.json()
        return None

    def resolve_conflicts(self):
        response = requests.get(f"{self.blockchain_url}/nodes/resolve")
        if response.status_code == 200:
            return response.json()
        return None

# Example usage:
# blockchain_interface = BlockchainInterface('localhost', 5000)
# blockchain_interface.new_transaction('sender_address', 'recipient_address', 100)
# blockchain_interface.mine_block()
# blockchain_interface.get_full_chain()
# blockchain_interface.register_node('http://192.168.0.5:5000')
# blockchain_interface.resolve_conflicts()

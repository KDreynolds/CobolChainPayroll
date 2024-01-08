import time

class Block:
    def __init__(self, index, timestamp, transactions, proof, previous_hash):
        """
        Initialize a new block in the blockchain.

        :param index: <int> The index of the block in the chain
        :param timestamp: <float> The time at which the block was created
        :param transactions: <list> A list of transactions included in the block
        :param proof: <int> The proof of work number that solves the mining puzzle
        :param previous_hash: <str> The hash of the previous block in the chain
        """
        self.index = index
        self.timestamp = timestamp
        self.transactions = transactions
        self.proof = proof
        self.previous_hash = previous_hash

    def to_dict(self):
        """
        Convert the block to a dictionary.

        :return: <dict> The block in a dictionary format
        """
        return {
            'index': self.index,
            'timestamp': self.timestamp,
            'transactions': self.transactions,
            'proof': self.proof,
            'previous_hash': self.previous_hash,
        }

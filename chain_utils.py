import hashlib
import json
from time import time

def hash_block(block):
    """
    Creates a SHA-256 hash of a Block

    :param block: <dict> Block
    :return: <str>
    """
    # We must make sure that the Dictionary is Ordered, or we'll have inconsistent hashes
    block_string = json.dumps(block, sort_keys=True).encode()
    return hashlib.sha256(block_string).hexdigest()

def valid_chain(chain):
    """
    Determine if a given blockchain is valid

    :param chain: <list> A blockchain
    :return: <bool> True if valid, False if not
    """
    last_block = chain[0]
    current_index = 1

    while current_index < len(chain):
        block = chain[current_index]
        print(f'{last_block}')
        print(f'{block}')
        print("\n-----------\n")
        # Check that the hash of the block is correct
        if block['previous_hash'] != hash_block(last_block):
            return False

        last_block = block
        current_index += 1

    return True

def create_genesis_block():
    """
    Create the genesis block and adds it to the chain

    :return: <dict> Genesis block
    """
    return {
        'index': 1,
        'timestamp': time(),
        'transactions': [],
        'proof': 100,
        'previous_hash': 1
    }

def proof_of_work(last_proof):
    """
    Simple Proof of Work Algorithm:
     - Find a number p' such that hash(pp') contains leading 4 zeroes
     - Where p is the previous proof, and p' is the new proof

    :param last_proof: <int>
    :return: <int>
    """
    proof = 0
    while valid_proof(last_proof, proof) is False:
        proof += 1

    return proof

def valid_proof(last_proof, proof):
    """
    Validates the Proof: Does hash(last_proof, proof) contain 4 leading zeroes?

    :param last_proof: <int> Previous Proof
    :param proof: <int> Current Proof
    :return: <bool> True if correct, False if not.
    """

    guess = f'{last_proof}{proof}'.encode()
    guess_hash = hashlib.sha256(guess).hexdigest()
    return guess_hash[:4] == "0000"

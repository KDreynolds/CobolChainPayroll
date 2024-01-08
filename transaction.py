class Transaction:
    def __init__(self, sender, recipient, amount):
        """
        Initialize a new transaction.

        :param sender: <str> Address of the Sender
        :param recipient: <str> Address of the Recipient
        :param amount: <float> Amount
        """
        self.sender = sender
        self.recipient = recipient
        self.amount = amount

    def to_dict(self):
        """
        Convert the transaction to a dictionary.

        :return: <dict> The transaction in a dictionary format
        """
        return {
            'sender': self.sender,
            'recipient': self.recipient,
            'amount': self.amount,
        }

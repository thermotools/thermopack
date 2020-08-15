class Component:
    def __init__(self, name, json_data):
        self.name = name
        self.json_data = json_data

    def __str__(self):
        return self.name

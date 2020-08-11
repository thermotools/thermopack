from PyQt5.QtWidgets import QTextBrowser
from gui.utils import MessageBox


class AboutWindow(QTextBrowser):
    """
    A window to show information about the Thermopack application
    The displayed info is the HTML content in the about.html file
    """

    def __init__(self, parent=None):
        super().__init__(parent=parent)
        self.setWindowTitle("About Thermopack")
        self.setMinimumSize(800, 600)
        self.setOpenExternalLinks(True)

        try:
            file = open("about.html", "r")
        except FileNotFoundError:
            msg = MessageBox("Error", "Could not open file containing the About Info")
            msg.exec_()
            return

        html = file.read()
        file.close()
        self.setHtml(html)

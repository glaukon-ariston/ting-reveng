import sublime, sublime_plugin

class EnumerateSelectionCommand(sublime_plugin.TextCommand):
    '''
    http://stackoverflow.com/questions/14574941/add-a-number-to-each-selection-in-sublime-text-2-incremented-once-per-selection

    Save the file (EnumerateSelection.py) to Data/Packages/User folder.
    Add this to Preferences|Key Bindings - User
    { "keys": ["ctrl+shift+e"], "command": "enumerate_selection" }
    '''
    def run(self, edit):
        start_value = int(self.view.substr(self.view.sel()[0]))

        format = '{-%2x-}'
        counter = 0
        for selection in self.view.sel():
            self.view.insert(edit, selection.begin(), format % (start_value + counter))
            counter = counter + 1

        for selection in self.view.sel():
            self.view.erase(edit, selection)

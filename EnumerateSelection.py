import sublime, sublime_plugin

class EnumerateSelectionCommand(sublime_plugin.TextCommand):
    '''
    http://stackoverflow.com/questions/14574941/add-a-number-to-each-selection-in-sublime-text-2-incremented-once-per-selection

    Save the file (EnumerateSelection.py) to Data/Packages/User folder.
    Add this to Preferences|Key Bindings - User
    { "keys": ["ctrl+shift+e"], "command": "enumerate_selection" }

    Example output can be seen in
    /app/dev/hp/projects/github/ting-reveng/lib/ting/src/Ting/Instrinctions.hs:1031 sanityCheckBasic

        ...
        $ {- 0-} [(set r0 0xa5a5 >> cmp r0 0xa5a5) >>. je
        , {- 1-} (cmp r0 0xface) >>. jne
        , {- 2-} (cmp r0 r0) >>. je
        , {- 3-} (set r0 r0 >> cmp r0 r0) >>. je
        , {- 4-} (adds r0 1 >> cmp r0 0xa5a6) >>. je
        , {- 5-} (adds r0 0 >> cmp r0 0xa5a6) >>. je
        , {- 6-} (subs r0 1 >> cmp r0 0xa5a5) >>. je
        , {- 7-} (subs r0 0 >> cmp r0 0xa5a5) >>. je
        , {- 8-} (and r0 0x00ff >> cmp r0 0x00a5) >>. je
        , {- 9-} (or r0 0xa500 >> cmp r0 0xa5a5) >>. je
        , {- a-} (not r0 >> cmp r0 0x5a5a) >>. je
        , {- b-} (set r1 0xa5a5 >> cmp r0 r1) >>. jb
        , {- c-} jbe
        , {- d-} cmp r1 r0 >>. jg
        , {- e-} jge
        , {- f-} (set rc 0x7ffe >> adds rc 1 >> cmp rc 0x7fff) >>. je
        , {-10-} (adds rc 1 >> cmp rc 0x8000) >>. je
        , {-11-} (adds rc 1 >> cmp rc 0x8001) >>. je
        , {-12-} (subs rc 2 >> cmp rc 0x7fff) >>. je
        , {-13-} (set rc 0xfffe >> adds rc 1 >> cmp rc 0xffff) >>. je
        ...

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

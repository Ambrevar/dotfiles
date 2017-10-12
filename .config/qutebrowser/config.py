c.auto_save.session = True
c.completion.shrink = True
c.confirm_quit = ["downloads"]
c.content.cache.size = 5242880
c.downloads.location.directory = "~/temp"
c.downloads.location.prompt = False
c.downloads.location.remember = True
c.editor.command = ["emacsclient", "{}"]
c.hints.scatter = False
c.hints.uppercase = True
c.input.partial_timeout = 2000
c.tabs.tabs_are_windows = True
c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "arch": "http://wiki.archlinux.org/index.php?title=Special%3ASearch&search={}&go=Go",
    "aur": "https://aur.archlinux.org/packages.php?O=0&K={}&do_Search=Go",
    "ctan": "http://www.ctan.org/search?phrase={}",
    "gd": "https://godoc.org/?q={}",
    "gentoo": "https://wiki.gentoo.org/index.php?title=Special%3ASearch&search={}&go=Go",
    "gi": "https://www.google.com/search?site=imghp&tbm=isch&source=hp&biw=1676&bih=997&q={}",
    "gm": "https://maps.google.com/maps?q={}",
    "gp": "http://localhost:6060/pkg/{}",
    "imdb": "http://www.imdb.com/find?q={}&s=all",
    "leo": "http://dict.leo.org/frde/index_de.html#/search={}",
    "mba": "http://musicbrainz.org/search?query={}&type=artist&method=indexed",
    "mbr": "http://musicbrainz.org/search?query={}&type=release&method=indexed",
    "so": "http://stackoverflow.com/search?q={}",
    "torrent": "https://kickass.unblocked.live/search.php?q={}",
    "tpb": "https://ukpirate.click/s/?q={}",
    "w": "http://en.wikipedia.org/wiki/Special:Search?search={}",
    "wb": "http://en.wikibooks.org/wiki/Special:Search?search={}",
    "wbf": "http://fr.wikibooks.org/wiki/Special:Search?search={}",
    "wf": "http://fr.wikipedia.org/wiki/Special:Search?search={}",
    "wk": "http://en.wiktionary.org/wiki/Special:Search?search={}",
    "wkf": "http://fr.wiktionary.org/wiki/Special:Search?search={}",
    "wks": "http://sv.wiktionary.org/wiki/Special:Search?search={}",
    "wine": "http://www.winehq.org/search/?cx=partner-pub-0971840239976722:w9sqbcsxtyf&cof=FORID:10&ie=UTF-8&q={}",
    "wr": "http://www.wordreference.com/enfr/{}",
    "yt": "http://www.youtube.com/results?search_query={}",
}

config.bind(',v', 'spawn mpv {url}')

## Uncomment this to still load settings configured via autoconfig.yml
# config.load_autoconfig()

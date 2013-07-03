#-----------------------------------------------------------------------------
# web objects

class webobj(str):
    mime = None
    ext = ''
    pass

class HTML(webobj):
    mime = "text/html"
    ext = '.html'
    pass

class JS(webobj):
    mime = "text/javascript"
    ext = '.js'
    pass

class PNG(webobj):
    mime = "image/png"
    ext = '.png'
    pass

class GIF(webobj):
    mime = "image/gif"
    ext = '.gif'
    pass

class SVG(webobj):
    mime = "text/xml"
    ext = '.svg'
    pass

mimetab = [HTML, JS, PNG, GIF, SVG]

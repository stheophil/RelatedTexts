RelatedTexts
============

A simple "related texts" matching engine for the [wahlversprechen](https://github.com/stheophil/wahlversprechen) project.

It builds simple text statistics over a list of items, e.g. summaries of election promises. 
It pulls and scrapes articles from RSS feeds and finds which election promises currently occur in the news. 

Still very much work in progress. 

**TODO:**

- When extracting article text, also get site icons:
  
  &lt;link href="..." rel="shortcut icon" type="image/ico"&gt;  
  &lt;link rel="apple-touch-icon" href="iphone.png" &gt;  
  &lt;link rel="apple-touch-icon" sizes="72x72" href="touch-icon-ipad.png" &gt;  
  &lt;link rel="apple-touch-icon" sizes="114x114" href="touch-icon-iphone-retina.png" &gt;  
  &lt;link rel="apple-touch-icon" sizes="144x144" href="touch-icon-ipad-retina.png" &gt;  

- keep track of parsed and rated texts with date when they have been seen first
- read this list at program start, store it at program end
- export rated matches as JSON object
- prune rated to only contain texts at most 24h|3d|7d old
- include URL and keywords in items (election promises), give keywords high weight automatically

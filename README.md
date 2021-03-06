
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kaz-cadastre-scrapper

<!-- badges: start -->

<!-- badges: end -->

The goal of kaz-cadastre-scrapper is to harvest raster and vector GIS
data alonge with the metadata from the Kazakhstan Geo Cadaster website:

  - [http://www.aisgzk.kz/aisgzk/ru/content/maps](http://www.aisgzk.kz/aisgzk/ru/content/maps?type=ug)

This is a tidious process, however, mapserver seems to contain some
information in poligonised format.

Generic access to the map servers API is available here:
<http://www.aisgzk.kz/aisgzk/Proxy/aisgzkZem2/MapServer/>. All links are
generally accessible through a Kaz in-built proxy.

Here is the detailed JS code that displays the map and could be used as
a reference for understing maps structure:
<http://www.aisgzk.kz/aisgzk/js/maps.js>

Some key links:

  - zu - cadastre shapes -
    [/aisgzk/Proxy/aisgzkZem2/MapServer](/aisgzk/Proxy/aisgzkZem2/MapServer)
    
      - zuPoint: ‘/aisgzk/Proxy/aisgzkZemPnt2/MapServer’,

  - ug - land classification (Karta Ugodij)
    [/aisgzk/Proxy/aisgzkUgL/MapServer](/aisgzk/Proxy/aisgzkUgL/MapServer)

  - oz - karta otsenochnih zon -
    [aisgzk/Proxy/aisgzkOz/MapServer](aisgzk/Proxy/aisgzkOz/MapServer)

  - ot - karta zemelnih otvodoc -
    [aisgzk/Proxy/aisgzkOT/MapServer](aisgzk/Proxy/aisgzkOT/MapServer)

  - rst - Geo botanical map
    [/aisgzk/Proxy/aisgzkRst/MapServer](/aisgzk/Proxy/aisgzkRst/MapServer)
    
      - rst\_kart: ‘/aisgzk/Proxy/aisgzkRst\_kart/MapServer’,

  - pch - soils map ‘/aisgzk/Proxy/aisgzkPch/MapServer’
    
      - pch\_kart: ‘/aisgzk/Proxy/aisgzkPch\_kart/MapServer’,

  - Sebserver for requests on land plots consolidation (no data)
    
      - puSP: ‘/aisgzk/Proxy/GPServer/SoglProj/’
      - puOOS: ‘/aisgzk/Proxy/GPServer/OpOcStoim/’
      - puOKS: ‘/aisgzk/Proxy/GPServer/OpKachSost/’

  - Geometry untility service: gs:
    ‘/aisgzk/Proxy/Utilities/Geometry/GeometryServer’,

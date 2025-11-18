# StreamDataPanel-HS

## Introduction

This is the Haskell Version of StreamDataPanel, which is a web app, used to show frequently-freshed data as line chart, bar chart, pie chart, radar chart, scatter chart or surface chart. It is based on eel with python, optimized for speed.

The original project is based on python, providing with a server and client, while this project is based purely on Haskell. You can start a server or client with Haskell natively.

To see the original project: [StreamDataPanel](https://github.com/SyuyaMurakami/StreamDataPanel)

To see the document of StreamDataPanel: [Document](https://streamdatapanel-doc.readthedocs.io/en/latest/index.html)

This document is based on python version, however, web design and page interaction is totally the same with those of haskell version.

## Install

Download this package by click the green ``Code`` button and click ``Download ZIP``, unzip it or use the following to clone this package:

    git clone https://github.com/SyuyaMurakami/StreamDataPanel-HS.git

After this, change directory to project directory:

    cd StreamDataPanel-HS-main

Use `cabal` to build package:

    cabal build

Then if no error is printed, it means success. You can continue to install it into your ghc:

    cabal install

If everything goes smoothly, two executable file should be added into your system, you can find the path where they are in your terminal. Do not forget to add this path into your shell config. If you already did this, just neglect this remind.

## Quick View

One picture is better than a thousand words:

<table width="100%">
  <tr>
    <td align="center" width="50%" >
      <img src="https://github.com/SyuyaMurakami/StreamDataPanel/raw/main/assets/1_low_res.gif" alt="GIF 1" style="width: 100%;">
    </td>
    <td align="center" width="50%" >
      <img src="https://github.com/SyuyaMurakami/StreamDataPanel/raw/main/assets/2_low_res.gif" alt="GIF 2" style="width: 100%;">
    </td>
  </tr>
</table>

## Quick Check

To see functions of StreamDataPanel, you can firstly run a test.

After installing it by ``cabal install``, use ``startSDP`` in terminal to run a test APP client. 

Then open another terminal and run ``simulateSDP`` to start a server sample.

By default, the web app should be running at http://localhost:9001. Visit it and type one of the following words below into ``ChartType`` input: ``line`` , ``bar`` , ``sequence`` , ``lines`` , ``bars`` , ``sequences`` , ``pie`` , ``radar`` , ``scatter`` , ``surface`` , ``area`` , ``areas``, ``gauge`` ``text``. Then type ``test`` into ``KeyWord`` input. Click ``Subscribe`` to see if it runs correctly. If You see a chart with data freshed every second, it means success.

## Usage Of Server

### Start API

The first thing is to import and start API. Import it by:

    import StreamDataPanel

Start API in your ``main :: IO ()`` by:

    start :: String -> Int -> String -> IO (DataMap, ThreadId)

It starts a background thread to allow users to register charts, and returns a register book and a thread id. The first parameter is host address, the second is port and the third is route path. If you do not change the client config, then you should always use port as ``9005``, use route as ``/ws``. They are default configs align with client. An example is as follows:

    (manager, tid) <- start "localhost" 9005 "/ws"

If you start API by ``start`` function, do not forget to kill the background thread before your ``main`` exit:

    killThread tid

You can also use ``with`` function to avoid manually kill thread every time you use it:

    with :: String -> Int -> String -> (DataMap -> IO ()) -> IO ()

This function accepts a handle to perform register logic and will automatically kill background thread when the handle function exits. An example is as follows:

    with "localhost" 9005 "/ws" handle

where the ``handle`` function is typed as:

    handle :: DataMap -> IO ()

This is totally the same as:

    (manager, tid) <- start "localhost" 9005 "/ws"
    handle manager
    killThread tid

### Register Data

Once you have a data manager, you can register your data through it. This action will tell web app that your data is ready to be subscribed. 

You will need a ``DataKey`` to mark your data stream. ``DataKey`` is defined as:

    type DataKey = (String, String)

The first string is used to tell what kind of charts this data should be showed by, the second string is the key words of subscription, it will be used as the title of your charts.  

To register your data, you will also need an initial value. Different chart type requires different data form.

    type NumberData = Double -- For line, bar, sequence
    type StringData = String -- For text
    type CoordinateData = (Double, Double) -- For scatter
    type DimensionData = ([String], [Double]) -- For pie, area, lines, bars, sequences
    type DimensionsData = ([String], [String], [[Double]]) -- For areas, 
    type SurfaceData = ((String, String, String), (Int, Int), [(Double, Double, Double)]) -- For surface
    type GaugeData = (String, (Double, Double), Double) -- For gauge
    type RadarData = ([String], [Double], [Double]) -- For radar

Then you can register your data stream with ``add`` function. It is typed:

    add :: (Streamable a) => DataMap -> DataKey -> a -> IO DataValue

An example is as follows:

    linesChart <- add manager ("lines", "test") (["A", "B"], [1.0, 2.0] :: [Double])

### Update Data

If you already have a ``DataValue`` variable, like ``linesChart`` in the example above, you can update your data by ``fresh`` function, which is typed as:

    fresh :: (Streamable a) => DataValue -> a -> IO ()

An example is like:

    linesChart `fresh` (["A", "B"], [1.7, 2.2] :: [Double])

Any web that subscribed this stream will automatically fresh the latest data you just put in.

## Usage Of Client

You can start web app by running the following in your **terminal**:

    startSDP

Be default, the app will be run at ``http://localhost:9001``. Open a browser and visit it. Again, we continue our example, type ``lines`` into ``ChartType`` input, then type ``test`` into ``KeyWord`` input, click ``Subscribe``. You should see your chart. After you fresh any data in your server, you should be able to see the change through your web app.

For detail of web interaction, you can refer to [Page Interaction](https://streamdatapanel-doc.readthedocs.io/en/latest/Web.html#page-interaction).


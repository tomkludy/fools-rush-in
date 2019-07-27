# fools-rush-in

## Purpose

Compares your current stock portfolio from Fidelity against Motley Fool missions, and tells you what you
need to do to get into complete alignment.

Everything this app does is local, it does not read anything nor send anything to any servers; you have
to give it its data each time you run it.


## Using

1. Download your Fidelity positions (csv file).

2. Open this web app and load the Fidelity positions csv file.  You should see your current
positions and some transactions that don't make much sense yet (as there's no target portfolio).

3. In another browser tab, log into fool.com and navigate to a mission portfolio page.  Copy the entire
page (Ctrl+A, Ctrl+C) and paste it into the `+ Add Target` tab.  A new tab will appear with the
mission portfolio positions.  Make sure it matches the fool.com website as the cut+paste is a bit wonky.
Repeat this process for each mission you want to follow; the app will automatically average targets across all
added mission portfolios.

4. Check the Transactions tab.  Selectively ignore any trades you don't want to execute and it will
continuously recalculate the rest.  You can also change your desired Cash position at the top if you
want.  Ignored trades are remembered in browser local storage so that you don't have to keep reselecting them.


## Building

Uses elm 0.19.

`elm make src/Main.elm --output=out/main.js`


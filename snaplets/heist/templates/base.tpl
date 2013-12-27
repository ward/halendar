<!DOCTYPE html>
<html>
    <head>
        <title><pagetitle /> :: Halendar - The Haskell Calendar</title>
        <link rel="stylesheet" type="text/css" href="/style.css">
        <meta charset="utf-8" />
        <link rel="icon" type="image/png" href="/favicon.png" />
    </head>
    <body>
        <header>
            <h1>
                <a href="/"><img src="/favicon.png" id="logo" alt="" /></a>
                Halendar
            </h1>
            <ifLoggedIn>
                <h4>Hello, <loggedInUser />! (<a href="/signout">Sign out</a>)</h4>
            </ifLoggedIn>
        </header>
        <apply template="navigation" />
        <div id="content">
            <h2><pagetitle /></h2>
            <apply-content />
        </div>
        <footer>By Ward Muylaert.</footer>
    </body>
</html>

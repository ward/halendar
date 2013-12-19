<nav>
    <ul>
        <li><a href="/">Home</a></li>
        <ifLoggedOut>
            <li><a href="/signup">Sign up</a></li>
            <li><a href="/signin">Sign in</a></li>
        </ifLoggedOut>
        <ifLoggedIn>
            <li><a href="/event/new">New event</a></li>
            <li><a href="/calendar">Calendar</a></li>
        </ifLoggedIn>
        <li><a href="/about">About</a></li>
    </ul>
</nav>

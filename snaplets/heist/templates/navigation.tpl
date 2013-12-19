<nav>
    <ul>
        <li><a href="/">Home</a></li>
        <ifLoggedOut>
            <li><a href="/signup">Sign up</a></li>
            <li><a href="/signin">Sign in</a></li>
        </ifLoggedOut>
        <ifLoggedIn>
            <li>Links to calendar and what not</li>
        </ifLoggedIn>
    </ul>
</nav>

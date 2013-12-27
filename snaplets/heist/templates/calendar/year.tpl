<apply template="base">
    <bind tag="pagetitle"><year /></bind>
    <p>
        <a href="/calendar/${prevyear}">prev</a>
        <a href="/calendar/${nextyear}">next</a>
    </p>
    <p><a href="/calendar/${year}/1">January</a>
        <a href="/calendar/${year}/2">February</a>
        <a href="/calendar/${year}/3">March</a>
        <a href="/calendar/${year}/4">April</a>
        <a href="/calendar/${year}/5">May</a>
        <a href="/calendar/${year}/6">June</a>
        <a href="/calendar/${year}/7">July</a>
        <a href="/calendar/${year}/8">August</a>
        <a href="/calendar/${year}/9">September</a>
        <a href="/calendar/${year}/10">October</a>
        <a href="/calendar/${year}/11">November</a>
        <a href="/calendar/${year}/12">December</a>
    </p>
    <ul>
    <events>
        <li><a href="/event/view/${eventid}"><eventtitle /></a>
            <ul>
                <li><eventstart /></li>
                <li><eventend /></li>
            </ul>
        </li>
    </events>
    </ul>
</apply>

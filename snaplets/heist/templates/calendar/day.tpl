<apply template="base">
    <bind tag="pagetitle"><year />-<month />-<day /></bind>
    <p>
        <a href="/calendar/${prevyear}/${prevmonth}/${prevday}">prev</a>
        <a href="/calendar/${nextyear}/${nextmonth}/${nextday}">next</a>
    </p>
    <p>
        <a href="/calendar/${year}/${month}">Month view</a>,
        <a href="/calendar/${year}">year view</a>
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

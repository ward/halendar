<apply template="base">
    <bind tag="pagetitle"><year /></bind>
    <p><a href="/calendar/${prevyear}">«</a>&nbsp;<a href="/calendar/${nextyear}">»</a></p>
    <p>List of the events this year.</p>
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

<apply template="base">
    <bind tag="pagetitle"><year />-<month /></bind>
    <p>List of the events this month.</p>
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

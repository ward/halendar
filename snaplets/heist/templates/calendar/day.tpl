<apply template="base">
    <bind tag="pagetitle"><year />-<month />-<day /></bind>
    <p>List of the events this day.</p>
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

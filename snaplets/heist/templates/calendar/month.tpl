<apply template="base">
    <bind tag="pagetitle"><year />-<month /></bind> <!-- TODO: Dynamically bind correct month -->
    <p>List of the events this month.</p>
    <ul>
    <events>
        <li><eventid /> - <eventtitle /> (<eventstart />-<eventend />)</li>
    </events>
    </ul>
</apply>

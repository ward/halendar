<apply template="base">
    <bind tag="pagetitle">View event</bind>
    <h3><eventtitle /> (<eventid />)</h3>
    <p>
        This event starts at <eventstart /> and ends at <eventend />.
        This event repeats <eventrepeat />.
    </p>
    <h4>Description</h4>
    <p><eventdescription /></p>
    <h4>Owner</h4>
    <p><eventowner /></p>
    <h4>Delete</h4>
    <form action="/event/delete/${eventid}" method="POST">
        <input type="submit" value="Delete event" />
    </form>
</apply>

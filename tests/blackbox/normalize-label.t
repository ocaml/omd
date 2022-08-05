Case insensitive comparison
  $ omd << "MD"
  > [ΑΓΩ]: /url
  > 
  > [αγω]
  > MD
  <p><a href="/url">αγω</a></p>

Collapse consecutive internal spaces, tabs
  $ omd << "MD"
  > [ΑΓ  	Ω]: /url
  > 
  > [αγ ω]
  > MD
  <p><a href="/url">αγ ω</a></p>

Strip leading and trailing spaces, tabs
  $ omd << "MD"
  > [ 	ΑΓΩ  ]: /url
  > 
  > [αγω]
  > MD
  <p><a href="/url">αγω</a></p>

Doesn't match due to the internal space
  $ omd << "MD"
  > [ΑΓΩ]: /url
  > 
  > [α γω]
  > MD
  <p>[α γω]</p>

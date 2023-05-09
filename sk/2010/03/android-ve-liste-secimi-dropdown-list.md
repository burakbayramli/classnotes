# Android ve Liste Secimi (Dropdown List)

Telefonde secenekli liste sunmak icin layout <Spinner
android:id="@+id/spinner1" android:layout_width="100dip"
android:layout_height="wrap_content" android:drawSelectorOnTop="true"
/>Activity icinde gerekli Java koduSpinner s1;protected void
onCreate(Bundle savedInstanceState) {
super.onCreate(savedInstanceState); ... s1 = (Spinner)
findViewById(R.id.spinner1); ArrayAdapter<String> adapter = new
ArrayAdapter<String> (this, android.R.layout.simple_spinner_item,
mStrings);
adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
s1.setAdapter(adapter);}private static final String[] mStrings = {
"deger1", "deger2", "deger3"};Secilen bir degerin ne oldugunu okumak
icin herhangi bir Listener icinde s1.getSelectedItem() cagrisi degeri
gerekli degeri getirecektir.






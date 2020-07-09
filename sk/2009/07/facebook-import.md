# Facebook Import


Facebook Import



Bir kullanicinin arkadas listesini, onlarin bilgisini kendi uygulamaniza yuklemek istiyorsaniz, alttaki kodlar yardimci olabilir. Once temel kodlar:  public Collection<String> getFriendIds() throws FacebookException, IOException {  Collection<String> userIds = new LinkedList<String>();  JSONArray friends = _client.friends_get();  if (friends == null || friends.length() == 0) {    return userIds;  }  /* Walk the list of result elements; each one's contents is a user ID. */  for (int i = 0; i < friends.length(); i++) {    try {      userIds.add(friends.getString(i));    }    catch (JSONException e) {      throw new IOException("friend list can't be extracted");    }  }  return userIds;}public JSONArray getInfo(Collection <String> userIds) {  Object result = null;  try {    // two fields    Set<ProfileField> fields = new HashSet<ProfileField>(2);    fields.add(ProfileField.UID);    fields.add(ProfileField.BIRTHDAY);    fields.add(ProfileField.FIRST_NAME);    fields.add(ProfileField.LAST_NAME);    fields.add(ProfileField.PIC_SMALL);    fields.add(ProfileField.SEX);    fields.add(ProfileField.RELATIONSHIP_STATUS);    fields.add(ProfileField.SIGNIFICANT_OTHER_ID);    fields.add(ProfileField.ACTIVITIES);    fields.add(ProfileField.INTERESTS);    fields.add(ProfileField.MUSIC);    fields.add(ProfileField.TV);    fields.add(ProfileField.MOVIES);    Collection<Long> longUserIds = new ArrayList<Long>(userIds.size());    for (String uid : userIds) {      longUserIds.add(Long.valueOf(uid));    }    result = _client.users_getInfo(longUserIds, fields);  } catch (Exception e) {    Util.log(e);  }  return (JSONArray)result;}Bu kodlari su sekilde cagirabiliriz (uid degiskeni kullanicinin facebook kimligini tasiyor olsun):  public void importUsers() {  try {    Collection<String> plist = new LinkedList<String>();    plist.add(uid);    JSONArray parr = getInfo(plist);    importUsers(parr);    Collection<String> list = getFriendIds();    JSONArray arr = getInfo(list);    importUsers(arr);  } catch (Exception e) {    Util.log(e);  }}public void importUsers(JSONArray arr) {  .. for (int i = 0; i < arr.length(); i++) {   JSONObject obj = (JSONObject)arr.get(i);   ...   value = ""+obj.get(ProfileField.BIRTHDAY.toString());   ... }}Session id uzerinden facebook kimliginin nasil alinacagini bir onceki yazida islemistik.





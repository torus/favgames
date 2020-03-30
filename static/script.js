console.log("hello")

function onlogin(res) {
  if (res.status == 'connected') {
    console.log("connected")
    $.ajax({
      method: 'POST',
      url: '/login',
      data: JSON.stringify({userID: res.authResponse.userID})
    }).done(data => {
      console.log(data)
    })
  } else {
    console.log("not connected", res)
  }
  // authResponse:
  // accessToken: "EAAGps5QyFhcBAGqkswuMrZAqWzSCFKESOuICmwdVCb8co1VRGDr5SoqIZCiKaiZCPyi8BkmkhaXqlyF6lRgAyLZA2qjAEXVsPrbZBNVVQFIXjTwgw3TJfYOIP1ZCSDQGMMCJxV4rPw0p1XEVl0CIICack6UBLZBhiHKdEEFXpELM23GDd5H82Kn6atUpEZANmet7EeFEvb7zQips9xBwLCXy"
  // userID: "10220058366229790"
  // expiresIn: 6719
  // signedRequest: "zP3xMMj0VhI6bvAhKmKpnvfpfurGNiW04TmTV0H4-JA.eyJ1c2VyX2lkIjoiMTAyMjAwNTgzNjYyMjk3OTAiLCJjb2RlIjoiQVFBNGx2UnFVZEVrYXVqQjVTYjVHcHk0VENaQk42OV9qSU9OYURROHZvdDZuLVk2TmVydjNieHhfVldzMTVqVHF1TzNHYjd4d2ZGREdMalc5UGRaNkU4aXhYOUcxSEhvZ20za0kyQmF4Rnl1TDNIbWc0c3JPaU1Nb1FVMFRhOG9keV8tN2FIUDNJNmFtMkFjMi04al81bW9IUWk2aDVEaHBLMXBFcFdoYXhXa2cxQk5yc2hKQ19SMEY0a3p2SUo2WDNvbzdNYTFYbmJJZ3ExMm1xSHpjNXRrOFBlbWxiVTRtRTItRDhKUEVSaXZUTEZUVjhTYzlwTGdLcGp6ZjRDQkU5QWIydUhwelYzS1FuRjNDSVBzV3FxMWltQWh6eXNfdGlrWEZrb1UzNnk2Yk5VTVdOQXQzbmpJWkJYckxmaGFld242dzJXblFPR19lR3lxWWtHUVhPSmJoVlkwMEUwVnpBVjBCc0UxRDZDRHlGdGpxRk41NXJxZWVBeFVRazM5TGFjIiwiYWxnb3JpdGhtIjoiSE1BQy1TSEEyNTYiLCJpc3N1ZWRfYXQiOjE1ODU0MDQ0ODF9"
  // graphDomain: "facebook"
  // data_access_expiration_time: 1593180481

}

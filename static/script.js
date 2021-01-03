console.log("hello")

function onlogin(res) {
    if (res.status == 'connected') {
        console.log("connected")

        const httpReq = new XMLHttpRequest()
        httpReq.onreadystatechange = () => {
            if (httpReq.readyState === XMLHttpRequest.DONE) {
                if (httpReq.status === 200) {
                    const data = httpReq.responseText
                    console.log(data)

                    const m = location.search.match(/^\?redirect=(\/[^&]+)/)
                    location.href = m && m[1] || '/'
                } else {
                    console.log("Error", httpReq.status)
                }
            } else {
                console.log("onreadystatechange", httpReq.readyState)
            }
        }
        httpReq.open('POST', '/login')
        httpReq.setRequestHeader('Content-Type', 'text/json')
        httpReq.send(JSON.stringify({userID: res.authResponse.userID}))
    } else {
        console.log("not connected", res)
    }
}

function ajax(method, url, data) {
    return new Promise((result, reject) => {
        const httpReq = new XMLHttpRequest()
        httpReq.onreadystatechange = () => {
            if (httpReq.readyState === XMLHttpRequest.DONE) {
                if (httpReq.status === 200) {
                    const data = httpReq.responseText
                    console.log(data)
                    result(data)
                } else {
                    console.log("Error", httpReq.status)
                    reject(httpReq.status)
                }
            } else {
                console.log("onreadystatechange", httpReq.readyState)
            }
        }
        httpReq.open(method, url)
        httpReq.setRequestHeader('Content-Type', 'text/json')
        httpReq.send(JSON.stringify(data))
    })
}

function addGame(gameId, buttonId) {
    let button = document.querySelector("#" + buttonId)
    button.setAttribute('disabled', 'disabled')
    button.innerText = '...'

    ajax('POST', '/add', {game: gameId})
        .then(data => {
            button.innerText = "追加済み"
        })
        .catch(err => {
            console.log("ERROR", err)
        })
}

function submitProfile(form) {
    const name = form['name-input'].value
    console.log("submitProfile", name)

    ajax('POST', '/profile/update', {name: name})
        .then(data => {
            console.log('submitted', data)

            const btn = form.querySelector('button')
            console.log("button", btn)

            btn.innerText = '保存済み'

            form.onchange = ev => {
                btn.removeAttribute('disabled')
                btn.innerText = '保存'
            }
        })
        .catch(err => {
            console.log("ERROR", err)
        })

    form.querySelector('button').setAttribute('disabled', 'disabled')

    return false
}

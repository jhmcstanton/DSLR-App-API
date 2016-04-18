## GET /api/all

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- Keyframe list with minimum info

```javascript
[[321,{"keyframes":[],"name":null}]]
```

- Keyframe list with minimum info, Keyframe list with minimum info

```javascript
[[321,{"keyframes":[],"name":null}],[321,{"keyframes":[],"name":null}]]
```

- Small, named keyframe

```javascript
[[321,{"keyframes":[{"tiltAngle":0,"time":0,"panAngle":0,"position":0}],"name":"My Starter Keyframe List"}]]
```

- Keyframe list with minimum info, Keyframe list with minimum info, Keyframe list with minimum info

```javascript
[[321,{"keyframes":[],"name":null}],[321,{"keyframes":[],"name":null}],[321,{"keyframes":[],"name":null}]]
```

## POST /api/new

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"keyframes":[],"name":null}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
null
```

- 

```javascript
321
```

## GET /api/single/:frameListID

#### Authentication



Clients must supply the following data


#### Captures:

- *frameListID*: (integer) keyframe list id in database

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
null
```

- Keyframe list with minimum info

```javascript
{"keyframes":[],"name":null}
```

- Small, named keyframe

```javascript
{"keyframes":[{"tiltAngle":0,"time":0,"panAngle":0,"position":0}],"name":"My Starter Keyframe List"}
```

- Multiple frames - the DSLR Dolly can generate transitions for this

```javascript
{"keyframes":[{"tiltAngle":0,"time":0,"panAngle":0,"position":0},{"tiltAngle":0,"time":30,"panAngle":0,"position":15},{"tiltAngle":30,"time":40,"panAngle":30,"position":15}],"name":"A few more frames"}
```

## GET /api/user/login

#### Authentication

HTTP BasicAuthentication


Clients must supply the following data
BasicAuthentication: username:password


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/octet-stream`

- Response body as below.

```
JWT
```

## POST /api/user/new

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
[{"email":"jim@pbjdollys.com","lastName":"Stanton","username":"jims_frames","firstName":"Jim"},"UserPassword"]
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
5432
```


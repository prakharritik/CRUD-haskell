
# Haskell Crud App

A simple CRUD app made in haskell.


## Tech Stack

Haskell, Servant, Persistant, pgsql


## API Reference

#### Register a user

```http
  POST /register
```

| Request Body | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `email` | `string` | **Required** |
| `name` | `string` | **Required** |
| `password` | `string` | **Required** |

Returns a jwt token.

#### Login a user

```http
  POST /login
```

| Request Body | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `email` | `string` | **Required** |
| `password` | `string` | **Required** |

Returns a jwt token.


#### Get all movies

```http
  GET /movies
```

Returns all movies.

#### Get Movie

```http
  GET /movies/${id}
```

| Parameter | Type     | Description                       |
| :-------- | :------- | :-------------------------------- |
| `id`      | `Number` | **Required**. Id of movie to fetch |


Authorization header containing Bearer JWT token  is required for below endpoints.

```http
  POST /movies
```

| Request Body | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `title` | `string` | **Required** |
| `description` | `string` | **Required** |

```http
  PUT /movies/${id}
```

| Parameter | Type     | Description                       |
| :-------- | :------- | :-------------------------------- |
| `id`      | `Number` | **Required**. Id of movie to update |

| Request Body | Type     | Description                |
| :-------- | :------- | :------------------------- |
| `title` | `string` | **Required** |
| `description` | `string` | **Required** |
 

```http
  DELETE /movies/${id}
```

| Parameter | Type     | Description                       |
| :-------- | :------- | :-------------------------------- |
| `id`      | `Number` | **Required**. Id of movie to delete |

#### add(num1, num2)

Takes two numbers and returns the sum.


## Environment Variables

To run this project, you will need to add the following environment variables to your .env file

`jwtSecret`


## Run Locally

Clone the project

```bash
  git clone https://github.com/prakharritik/CRUD-haskell
```

Go to the project directory

```bash
  cd CRUD-haskell
```

Build the project

```bash
  stack build
```

Start the server

```bash
  stack run
```


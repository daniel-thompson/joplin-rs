// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2023 Daniel Thompson

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::io::Write as IoWrite;
use std::path::Path;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Request failed")]
    RequestFailed,

    #[error("environment error")]
    Env(#[from] std::env::VarError),

    #[error("file I/O error")]
    Io(#[from] std::io::Error),

    #[error("serialization failure")]
    Json(#[from] serde_json::Error),

    #[error("joplin API failure")]
    Reqwest(#[from] reqwest::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Client {
    client: reqwest::blocking::Client,
    uri: String,
    token: String,
}

impl Client {
    pub fn new(uri: &str, token: &str) -> Self {
        Self {
            client: reqwest::blocking::Client::new(),
            uri: uri.to_string(),
            token: token.to_string(),
        }
    }

    fn get(&self, uri: &str) -> Result<Value> {
        //println!("Fetching {uri}");
        let mut page: Page = self.client.get(uri).send()?.json()?;
        let mut items = page.items;

        let mut page_num = 1;

        while page.has_more {
            let req = format!("{uri}&page={page_num}");
            //println!("Fetching {req}");
            page = self.client.get(req).send()?.json()?;

            items.append(&mut page.items);
            page_num += 1;
            assert!(page_num < 250);
        }

        Ok(Value::Array(items))
    }

    pub fn client(&self) -> &reqwest::blocking::Client {
        &self.client
    }

    pub fn get_folders(&self) -> Result<Vec<Note>> {
        let uri = format!("{}/folders?token={}", self.uri, self.token);
        let data: Vec<Note> = serde_json::from_value(self.get(&uri)?)?;
        Ok(data)
    }

    pub fn delete_note(&self, id: &str) -> Result<()> {
        let uri = format!("{}/notes/{}?token={}", self.uri, id, self.token);
        let res = self.client.delete(uri).send()?;

        if res.status() == 200 {
            Ok(())
        } else {
            Err(Error::RequestFailed)
        }
    }

    pub fn get_note(&self, id: &str) -> Result<NoteWithBody> {
        let uri = format!(
            "{}/notes/{id}?token={}&fields=id,parent_id,title,body,updated_time",
            self.uri, self.token
        );
        let mut note: NoteWithBody = self.client.get(uri).send()?.json()?;

        // Joplin uses "Unix timestamps in milliseconds"... let's convert this
        // into "real" Unix timestamps
        note.updated_time /= 1000.0;

        Ok(note)
    }

    pub fn get_notes(&self) -> Result<Vec<Note>> {
        let uri = format!("{}/notes?token={}", self.uri, self.token);
        let data: Vec<Note> = serde_json::from_value(self.get(&uri)?)?;
        Ok(data)
    }

    pub fn search_notes(&self, needle: &str) -> Result<Vec<Note>> {
        let needle = needle.replace(" ", "%20");
        let uri = format!("{}/search?query={}&token={}", self.uri, needle, self.token);
        let mut data: Vec<Note> = serde_json::from_value(self.get(&uri)?)?;
        data.sort_by(|a, b| a.id.cmp(&b.id));
        data.dedup_by(|a, b| a.id == b.id);
        Ok(data)
    }

    pub fn delete_resource(&self, id: &str) -> Result<()> {
        let uri = format!("{}/resources/{}?token={}", self.uri, id, self.token);
        let res = self.client.delete(uri).send()?;

        if res.status() == 200 {
            Ok(())
        } else {
            Err(Error::RequestFailed)
        }
    }

    pub fn get_resources(&self) -> Result<Vec<Resource>> {
        let uri = format!("{}/resources?token={}", self.uri, self.token);
        let mut data: Vec<Resource> = serde_json::from_value(self.get(&uri)?)?;

        // HACK: we seem to be getting duplicates when we fetch the resources
        data.sort_by(|a, b| a.id.cmp(&b.id));
        data.dedup_by(|a, b| a.id == b.id && a.title == b.title);

        Ok(data)
    }

    pub fn post_file(&self, desc: ResourceTemplate, path: impl AsRef<Path>) -> Result<Resource> {
        let form = reqwest::blocking::multipart::Form::new()
            .text("props", serde_json::to_string(&desc)?)
            .file("data", path)?;

        let res = self
            .client
            .post(format!("{}/resources?token={}", self.uri, self.token))
            .multipart(form)
            .send()?;

        if res.status() == 200 {
            Ok(Resource {
                id: desc.id.unwrap(),
                title: desc.title,
            })
        } else {
            Err(Error::RequestFailed)
        }
    }

    /// TODO: terrible API! Should return binary data but I don't
    ///       want to fake that whilst the reqwest guts can't handle
    ///       such data.
    pub fn get_resource(&self, id: &str) -> Result<String> {
        Ok(self
            .client
            .get(format!(
                "{}/resources/{}/file?token={}",
                self.uri, id, self.token
            ))
            .send()?
            .text()?)
    }

    /// TODO: Broken in a similar way to get_file()
    /// TODO: it's cheating to use a temporary file since we could do this
    ///       all in-memory if we could just figure out how to post the
    ///       resource correctly (code below causes Joplin give a bad
    pub fn update_resource(&self, id: &str, data: &str) -> Result<()> {
        let mut f = tempfile::Builder::new()
            .prefix("joplin-todo-")
            .rand_bytes(8)
            .suffix(".eml")
            .tempfile()?;
        write!(f, "{data}")?;
        f.flush()?;
        let form = reqwest::blocking::multipart::Form::new()
            .text("props", "{}")
            .file("data", f.path())?;

        let res = self
            .client
            .put(format!(
                "{}/resources/{}?token={}",
                self.uri, id, self.token
            ))
            .multipart(form)
            .send()?;

        if res.status() == 200 {
            Ok(())
        } else {
            Err(Error::RequestFailed)
        }
    }

    pub fn post_note(&self, desc: NoteTemplate) -> Result<Note> {
        let id = if let Some(id) = &desc.id {
            id.clone()
        } else {
            let id = uuid::Uuid::new_v4();
            id.simple().to_string()
        };

        let res = self
            .client
            .post(format!("{}/notes?token={}", self.uri, self.token))
            .json(&desc)
            .send()?;

        if res.status() == 200 {
            Ok(Note {
                id,
                parent_id: "".to_string(),
                title: desc.title,
            })
        } else {
            Err(Error::RequestFailed)
        }
    }

    pub fn update_note(&self, note: &NoteWithBody) -> Result<()> {
        let res = self
            .client
            .put(format!(
                "{}/notes/{}?token={}",
                self.uri, note.id, self.token
            ))
            .json(&note)
            .send()?;

        if res.status() == 200 {
            Ok(())
        } else {
            Err(Error::RequestFailed)
        }
    }

    /// TODO: should be generic (anything that can become bytes)
    pub fn post_resource(&self, desc: ResourceTemplate, data: &str) -> Result<Resource> {
        let mut f = tempfile::Builder::new()
            .prefix("joplin-todo-")
            .rand_bytes(8)
            .suffix(".eml")
            .tempfile()?;
        write!(f, "{data}")?;
        f.flush()?;
        self.post_file(desc, f.path())

        /*
                let form = reqwest::blocking::multipart::Form::new()
                    .text("props", serde_json::to_string(&desc)?)
                    .part(
                        "data",
                        reqwest::blocking::multipart::Part::text(data.to_string())
                            .mime_str("message/rfc822")?,
                    );

                let res = self
                    .client
                    .post(format!("{}/resources?token={}", self.uri, self.token))
                    .multipart(form)
                    .send()?;

                if res.status() == 200 {
                    Ok(Resource {
                        id: desc.id.unwrap(),
                        title: desc.title,
                    })
                } else {
                    dbg!(res);
                    Err(Error::RequestFailed)
                }
        */
    }
}

/// TODO: this doesn't need to stay public!
#[derive(Debug, Serialize, Deserialize)]
pub struct Page {
    has_more: bool,
    items: Vec<serde_json::Value>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NoteTemplate {
    pub id: Option<String>,
    pub parent_id: Option<String>,
    pub title: String,
    pub is_todo: i32,
    pub body: Option<String>,
}

impl NoteTemplate {
    pub fn new(title: &str) -> Self {
        Self {
            id: None,
            parent_id: None,
            title: title.to_string(),
            is_todo: 0,
            body: None,
        }
    }

    /// TODO: macro?
    pub fn body(self, body: String) -> Self {
        Self {
            body: Some(body),
            ..self
        }
    }

    pub fn todo(self) -> Self {
        Self { is_todo: 1, ..self }
    }

    pub fn parent_id(self, parent_id: String) -> Self {
        Self {
            parent_id: Some(parent_id),
            ..self
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Note {
    pub id: String,
    pub parent_id: String,
    pub title: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NoteWithBody {
    pub id: String,
    pub parent_id: String,
    pub title: String,
    pub body: String,
    pub updated_time: f64,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ResourceTemplate {
    pub id: Option<String>,
    pub title: String,
    pub mime: Option<String>,
}

impl ResourceTemplate {
    pub fn new(title: &str) -> ResourceTemplate {
        let id = uuid::Uuid::new_v4();
        Self {
            id: Some(id.simple().to_string()),
            title: title.to_string(),
            mime: None,
        }
    }

    /// TODO: macro?
    pub fn mime(self, mimetype: &str) -> Self {
        Self {
            mime: Some(mimetype.to_string()),
            ..self
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Resource {
    pub id: String,
    pub title: String,
}

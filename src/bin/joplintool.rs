// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2023 Daniel Thompson

use clap::{Parser, Subcommand};
use mail_parser::{Message, MessageParser};
use serde::{Deserialize, Serialize};
use std::cmp::min;
use std::fmt::Write as FmtWrite;
use std::fs;
use std::io;
use std::io::Write as IoWrite;
use std::path::{Path, PathBuf};
use thiserror::Error;
use time::macros::format_description;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Bad environment variables")]
    BadEnvironment,

    #[error("Cannot parse configuration file")]
    ConfigParseError(#[from] toml::de::Error),

    #[error("This command is disabled and cannot be run")]
    DisabledCommand,

    #[error("IO error")]
    IOError(#[from] io::Error),

    #[error("Joplin backend error: {0}")]
    JoplinError(#[from] joplin::Error),

    #[error("Cannot parse input as mailbox")]
    MailboxParseFailed,

    #[error("No results matching `{0}`")]
    NoResults(String),

    #[error("Exit code {0:?}")]
    ProcessExitCode(Option<i32>),

    #[error("Invalid regular expression: {0}")]
    RegexError(#[from] regex::Error),

    #[error(transparent)]
    TimeFormat(#[from] time::error::Format),

    #[error(transparent)]
    TimeComponentRange(#[from] time::error::ComponentRange),

    #[error("Too many results matching `{0}`")]
    TooManyResults(String),
}

/// Extension trait to allow std::process::ExitStatus to be easily
/// converted into a Result.
///
/// Will be obsoleted if
/// [exit_status_error](https://doc.rust-lang.org/std/process/struct.ExitStatusError.html)
/// is stablized.
trait ExitStatusExt {
    fn ok(&self) -> Result<()>;
}

impl ExitStatusExt for std::process::ExitStatus {
    fn ok(&self) -> Result<()> {
        if self.success() {
            Ok(())
        } else {
            Err(Error::ProcessExitCode(self.code()))
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Serialize, Deserialize)]
struct Config {
    token: String,

    #[serde(default = "default_uri")]
    uri: String,
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Summarize the actions provided for the specified subject
    Actions { subject: String },

    /// Tools to help clean up orphaned resources
    Clean {
        /// Only select e-mails (which are frequently orphaned because
        /// they are the basis of the TODO features
        #[arg(short, long)]
        email_only: bool,

        /// Automatically delete orphaned resources
        #[arg(short, long)]
        remove: bool,
    },

    /// Migrate completed TODO items into diary entries
    Diary {
        /// Show diary entries from the last week
        #[arg(long)]
        last_week: bool,
    },

    /// Preview the contents of an action
    Dryrun {
        /// The subject to lookup the action from
        subject: String,

        /// The action to be run
        action: String,
    },

    /// Edit a knowledge base entry
    Edit { subject: String },

    /// Search Joplin notes using a regular expressions
    Grep {
        /// Ignore  case  distinctions in patterns and input data
        #[arg(short, long)]
        ignore_case: bool,

        /// Pattern to search for
        regex: String,
    },

    /// Convert the knowledge base entry to HTML
    Html { subject: String },

    #[command(external_subcommand)]
    ImplicitRun(Vec<String>),

    /// Search Joplin using keywords
    Search {
        /// Keywords to search for, can include shell-like globs
        words: Vec<String>,
    },

    /// Show a knowledge base entry
    Show { subject: String },

    /// Run an action from the knowledge base
    Run {
        /// The subject to lookup the action from
        subject: String,

        /// The action to be run
        action: String,

        /// Optionally provide arguments for the action
        args: Vec<String>,
    },

    /// Create a new TODO item, either from stdin or from the supplied mbox
    Todo {
        /// Path to the mbox to add as a TODO
        mbox: Option<PathBuf>,
    },
}

fn main() {
    let res = app();
    if let Err(e) = res {
        // if we failed due to a sub-process error then we'll assume
        // the sub-process already explained the error so we can simply
        // exit immediately, with a matching error code
        if let Error::ProcessExitCode(code) = e {
            if let Some(code) = code {
                std::process::exit(code);
            }
        }

        // otherwise, display the error and exit with a failure code
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn app() -> Result<()> {
    let cli = Cli::parse();
    let config = match read_config() {
        Ok(config) => config,
        Err(e) => {
            eprintln!("ERROR: Configuration file cannot be loaded!\n{}", e);
            std::process::exit(1);
        }
    };
    let client = joplin::Client::new(&config.uri, &config.token);

    match cli.command {
        Commands::Actions { subject } => do_actions(&client, &subject)?,
        Commands::Clean { email_only, remove } => do_clean(&client, email_only, remove)?,
        Commands::Diary { last_week } => do_diary(&client, last_week)?,
        Commands::Dryrun { subject, action } => do_dryrun(&client, &subject, &action)?,
        Commands::Edit { subject } => do_edit(&client, &subject)?,
        Commands::Grep { ignore_case, regex } => do_grep(&client, &regex, ignore_case)?,
        Commands::ImplicitRun(args) => do_implicitrun(&client, args)?,
        Commands::Html { subject } => do_html(&client, &subject)?,
        Commands::Show { subject } => do_show(&client, &subject)?,
        Commands::Search { words } => do_search(&client, &words)?,
        Commands::Run {
            subject,
            action,
            args,
        } => do_run(&client, &subject, &action, &args)?,
        Commands::Todo { mbox } => do_todo(&client, mbox)?,
    }

    Ok(())
}

fn default_uri() -> String {
    "http://127.0.0.1:41184".to_string()
}

fn read_config() -> Result<Config> {
    let home = std::env::var_os("HOME").ok_or(Error::BadEnvironment)?;

    let configfile = Path::new(&home).join(".joplin").join("Joplin.toml");
    let config = fs::read_to_string(configfile)?;
    let config = toml::from_str(&config)?;

    Ok(config)
}

//
// Subcommand implementations
//

fn do_actions(client: &joplin::Client, subject: &str) -> Result<()> {
    let note = get_note_matching(client, subject)?;
    let actions = get_actions_from_note(&note);

    println!("Available actions for {}:", note.title);
    for action in actions.iter() {
        println!("    {action}");
    }

    Ok(())
}

fn do_clean(client: &joplin::Client, email_only: bool, remove: bool) -> Result<()> {
    let resources = client.get_resources()?;
    let mut got_one = false;
    for resource in resources.iter() {
        if client.search_notes(&resource.id)?.len() == 0 {
            if email_only && !resource.title.ends_with(".eml") {
                continue;
            }
            got_one = true;
            println!("{} {}", resource.id, resource.title);
            if remove {
                client.delete_resource(&resource.id)?;
            }
        }
    }

    if got_one {
        if !remove {
            println!(
                "\
WARNING: Cleaning is a dangerous operation. If resources/notes are
         partially synced then there is a risk data loss. Please
         review the above carefully before cleaning with --remove."
            );
        }
    } else {
        println!("Everything is clean!");
    }

    Ok(())
}

fn do_diary(client: &joplin::Client, last_week: bool) -> Result<()> {
    let summary = client.search_notes("iscompleted:1 notebook:TODO type:todo")?;

    // fetch the complete note and sort by updated time (this ensures that
    // the diary entries end up in time order)
    let mut notes = summary
        .into_iter()
        .map(|n| client.get_note(&n.id))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    notes.sort_by(|a, b| a.updated_time.total_cmp(&b.updated_time));

    for note in notes.iter() {
        let date = time::OffsetDateTime::from_unix_timestamp(note.updated_time as i64)?;
        let diary_date = date.format(&time::format_description::well_known::Iso8601::DATE)?;

        let diary = get_note_matching(client, &diary_date);

        let markdown = format!("- [x] {}\n", note.title);

        if let Ok(mut diary) = diary {
            println!("Append to {}: {}", diary.title, note.title);
            diary.body = diary.body + &markdown;
            client.update_note(&diary)?;
        } else {
            println!("Create {}: {}", diary_date, note.title);
            let folder_id = get_folder_id(client, "Diary")?;
            let note = joplin::NoteTemplate::new(&diary_date)
                .parent_id(folder_id.to_string())
                .body(markdown);

            let _ = client.post_note(note)?;
        }

        client.delete_note(&note.id)?;
    }

    if last_week {
        // TODO: "The system's UTC offset could not be determined"
        //let now = time::OffsetDateTime::now_local()?;
        let now = time::OffsetDateTime::now_utc();
        let this_monday =
            time::Date::from_iso_week_date(now.year(), now.iso_week(), time::Weekday::Monday)?;
        let prev_sunday: time::Date = this_monday - time::Duration::days(8);
        for day in 0..7 {
            let date: time::Date = prev_sunday + time::Duration::days(day);
            let date = date.format(&time::format_description::well_known::Iso8601::DATE)?;
            let note = get_note_matching(client, &date);
            if let Ok(note) = note {
                print!("# {date}\n\n{}\n\n", note.body);
            }
        }
    }

    Ok(())
}

fn do_dryrun(client: &joplin::Client, subject: &str, action: &str) -> Result<()> {
    let note = get_note_matching(client, subject)?;
    let script = get_action_from_note(&note, action)?;
    println!("{script}");
    Ok(())
}

fn do_edit(client: &joplin::Client, subject: &str) -> Result<()> {
    let mut note = get_note_matching(client, subject)?;

    let mut f = tempfile::Builder::new()
        .prefix(&format!("{} ", note.title))
        .rand_bytes(8)
        .suffix(".md")
        .tempfile()?;
    f.write(note.body.as_bytes())?;
    f.flush()?;

    let shell = match std::env::var("EDITOR") {
        Ok(shell) => shell,
        Err(_) => "vi".to_string(),
    };
    let status = std::process::Command::new(shell)
        .arg(f.path())
        .spawn()?
        .wait()?
        .ok();

    if let Ok(_) = status {
        // reload and update the note
        let f = f.reopen()?;
        let new_body = io::read_to_string(f)?;
        if note.body != new_body {
            println!("Updating {}", note.title);
            note.body = new_body;
            client.update_note(&note)?;
        }
    }
    status
}

fn do_grep(client: &joplin::Client, needle: &str, ignore_case: bool) -> Result<()> {
    let re = regex::RegexBuilder::new(needle)
        .case_insensitive(ignore_case)
        .build()?;

    let mut notes = client.get_notes()?;
    notes.sort_by(|a, b| a.title.cmp(&b.title));
    for note in notes {
        let note = client.get_note(&note.id)?;
        for ln in note.body.lines() {
            if re.is_match(ln) {
                println!("{}: {}", note.title, ln);
            }
        }
    }

    Ok(())
}

fn do_html(client: &joplin::Client, subject: &str) -> Result<()> {
    let note = get_note_matching(client, subject)?;

    let mut f = tempfile::NamedTempFile::new()?;
    f.write(note.body.as_bytes())?;
    f.flush()?;

    std::process::Command::new("cmark")
        .arg(f.path())
        .spawn()?
        .wait()?
        .ok()
}

fn do_implicitrun(client: &joplin::Client, args: Vec<String>) -> Result<()> {
    let subject = args.get(1);
    let action = &args[0];
    let args = &args[min(args.len(), 2)..];

    if let Some(subject) = subject {
        return do_run(&client, &subject, action, args);
    }

    if let Some(dirname) = std::env::current_dir()?.file_name() {
        if let Some(subject) = dirname.to_str().and_then(|n| n.rsplit('-').next()) {
            let res = do_run(&client, subject, action, args);
            if res.is_ok() {
                return res;
            }
        }
    }

    do_run(&client, "misc", action, args)
}

fn do_run(client: &joplin::Client, subject: &str, action: &str, args: &[String]) -> Result<()> {
    let note = get_note_matching(client, subject)?;
    let script = get_action_from_note(&note, action)?;

    let mut f = tempfile::NamedTempFile::new()?;

    // We cannot pass -ex on the bash command line since, in recent back releases, that results
    // in trace messages during initialization. This approach messes up line numbers but is the
    // less of the two evils.
    writeln!(f, "set -ex")?;
    writeln!(f, "{script}")?;
    f.flush()?;

    let shell = match std::env::var("SHELL") {
        Ok(shell) => shell,
        Err(_) => "sh".to_string(),
    };
    std::process::Command::new(shell)
        .arg(f.path())
        .args(args)
        .spawn()?
        .wait()?
        .ok()
}

fn do_search(client: &joplin::Client, words: &[String]) -> Result<()> {
    let notes = client.search_notes(&words.join(" "))?;
    for note in notes.iter() {
        println!("{}", note.title);
    }
    Ok(())
}

fn do_show(client: &joplin::Client, subject: &str) -> Result<()> {
    let note = get_note_matching(client, subject)?;
    print!("{}", note.body);
    Ok(())
}

fn do_todo<P: AsRef<Path>>(client: &joplin::Client, mbox: Option<P>) -> Result<()> {
    let raw = if let Some(fname) = mbox {
        fs::read_to_string(fname)?
    } else {
        io::read_to_string(std::io::stdin())?
    };

    let msg = MessageParser::default()
        .parse(&raw)
        .ok_or(Error::MailboxParseFailed)?;

    // Convert to mbox format (if needed)
    let mbox = if !raw.starts_with("From ") {
        let hdr = to_mbox_header(&msg).ok_or(Error::MailboxParseFailed)?;
        format!("{hdr}\n{raw}\n")
    } else {
        raw.to_string()
    };

    let subject = nibble_subject(msg.subject().unwrap_or("[empty subject]"));
    let resource_title = format!("{subject}.eml");
    let folder_id = get_folder_id(client, "TODO")?;

    let resource_id = get_resource_id(client, &resource_title);
    if let Ok(resource_id) = resource_id {
        println!("Appending to resource: {resource_title}");
        let mbox = client.get_resource(&resource_id)? + &mbox;
        client.update_resource(&resource_id, &mbox)?;

        // Do we need to re-create the todo item
        let needle = format!("{} iscompleted:0 notebook:TODO type:todo", resource_id);
        if client.search_notes(&needle)?.len() == 0 {
            create_note_from_message(client, &folder_id, &msg, &resource_id)?;
        }

        return Ok(());
    }

    if let Error::TooManyResults(_) = resource_id.unwrap_err() {
        println!("Too many resources for: {resource_title}.");
        println!("WARNING: Will have to create a new TODO item");
    }

    println!("Creating new resource: {resource_title}");
    let template = joplin::ResourceTemplate::new(&resource_title);
    let resource = client.post_resource(template, &mbox)?;

    create_note_from_message(client, &folder_id, &msg, &resource.id)?;

    Ok(())
}

//
// Joplin utility functions
//

fn create_note_from_message(
    client: &joplin::Client,
    folder_id: &str,
    msg: &Message,
    resource_id: &str,
) -> Result<()> {
    let mut note_body = String::new();
    writeln!(note_body, "[Open in mail application](:/{})\n", resource_id).unwrap();
    let name = msg
        .from()
        .and_then(|a| a.first())
        .and_then(|a| a.name())
        .unwrap_or("");
    let email = msg
        .from()
        .and_then(|a| a.first())
        .and_then(|a| a.address())
        .unwrap_or("");
    let address = format!("{name} <{email}>");

    let date = msg.date().map(|d| d.to_rfc822()).unwrap_or("".to_string());
    writeln!(note_body, "**From:** *{}*", address.trim()).unwrap();
    writeln!(note_body, "**Date:** *{date}*").unwrap();
    writeln!(
        note_body,
        "{}",
        msg.body_preview(1000)
            .unwrap_or(std::borrow::Cow::Borrowed("<HTML only e-mail>"))
    )
    .unwrap();

    let subject = nibble_subject(msg.subject().unwrap_or("[empty subject]"));
    println!("Creating new TODO item: {subject}");
    let note = joplin::NoteTemplate::new(subject)
        .parent_id(folder_id.to_string())
        .todo()
        .body(note_body);

    let _ = client.post_note(note)?;

    Ok(())
}

fn get_actions<'x>(body: &'x str, tag: &'x str) -> impl Iterator<Item = (&'x str, &'x str)> + 'x {
    body.split("\n~~~").filter_map(move |blk| {
        let blk = blk.trim_start();
        if blk.starts_with(tag)
            && blk.chars().nth(tag.chars().count()) == Some(' ')
            && blk.contains("\n")
        {
            let mut iter = blk[tag.len() + 1..].splitn(2, "\n");
            // unwrap() will never fail (because we checked s contains a newline)
            Some((iter.next().unwrap(), iter.next().unwrap()))
        } else {
            None
        }
    })
}

fn get_actions_from_note(note: &joplin::NoteWithBody) -> Vec<&str> {
    get_actions(&note.body, "sh").map(|(tag, _)| tag).collect()
}

fn get_action_from_note<'x>(note: &'x joplin::NoteWithBody, action: &str) -> Result<&'x str> {
    let actions: std::collections::HashMap<&str, &str> = get_actions(&note.body, "sh").collect();

    Ok(actions
        .get(action)
        .ok_or(Error::NoResults(action.to_string()))?)
}

fn get_note_matching(client: &joplin::Client, subject: &str) -> Result<joplin::NoteWithBody> {
    let mut notes = client.get_notes()?;
    notes.sort_by(|a, b| a.title.cmp(&b.title));
    let mut best_match = notes.binary_search_by(|note| note.title.as_str().cmp(subject));

    if let Err(off) = best_match {
        let lsubject = subject.to_lowercase();

        // try a case-insensitive match
        best_match = notes
            .iter()
            .enumerate()
            .filter(|(_, note)| note.title.to_lowercase() == lsubject)
            .next()
            .map(|(i, _)| i)
            .ok_or(0);

        // fall back to a case-sensitive prefix match
        if let Err(_) = best_match {
            if notes[off].title.starts_with(&subject) {
                best_match = Ok(off);
            }
        }

        // last chance... a case-insensitive prefix match
        if let Err(_) = best_match {
            best_match = notes
                .iter()
                .enumerate()
                .filter(|(_, note)| note.title.to_lowercase().starts_with(&lsubject))
                .next()
                .map(|(i, _)| i)
                .ok_or(0);
        }
    }

    if let Ok(i) = best_match {
        Ok(client.get_note(&notes[i].id)?)
    } else {
        Err(Error::NoResults(subject.to_string()))?
    }
}

fn get_folder_id(client: &joplin::Client, name: &str) -> Result<String> {
    let folders = client.get_folders()?;
    let folder = folders
        .iter()
        .enumerate()
        .find_map(|(i, f)| if f.title == name { Some(i) } else { None })
        .ok_or(Error::NoResults(name.to_string()))?;
    Ok(folders[folder].id.clone())
}

fn get_resource_id(client: &joplin::Client, title: &str) -> Result<String> {
    let resources = client.get_resources()?;
    let matches = resources
        .iter()
        .filter(|r| r.title == title)
        .collect::<Vec<_>>();

    if matches.len() > 1 {
        Err(Error::TooManyResults(title.to_string()))
    } else if matches.len() < 1 {
        Err(Error::NoResults(title.to_string()))
    } else {
        Ok(matches[0].id.clone())
    }
}

//
// mbox and mailer utility functions
//

fn nibble_subject(subject: &str) -> &str {
    let mut subject = subject;

    'outer: loop {
        for prefix in ["Re: ", "RE: ", "Fwd: ", "Fw: ", "FW: "].iter() {
            if subject.starts_with(prefix) {
                subject = &subject[prefix.len()..];
                continue 'outer;
            }
        }
        break;
    }

    subject
}

/// Implement as an extention trait?
fn to_mbox_header(msg: &Message) -> Option<String> {
    let email = msg.from()?.first()?.address()?;
    let date = msg.date()?;

    // format as UTC asctime (e.g. Www Mmm dd hh:mm:ss yyyy)
    let format = format_description!(
        "[weekday repr:short] [month repr:short] [day padding:space] [hour]:[minute]:[second] [year]"
    );
    let date = time::OffsetDateTime::from_unix_timestamp(date.to_timestamp()).ok()?;

    Some(format!("From {} {}", email, date.format(format).ok()?))
}

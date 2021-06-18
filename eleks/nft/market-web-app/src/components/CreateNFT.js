import { connect } from 'react-redux';
import { compose, withState, withHandlers, withProps } from 'recompose';

import { formatForAPI } from '../helpers/utils';
import { getMyTokensFetching } from '../reducers';
import { fetchAddToken } from '../actions/myTokens';

import Loader from './Loader';
import Form from 'react-bootstrap/Form';
import DropzoneComponent from './Dropzone';
import Button from 'react-bootstrap/Button';

import '../styles/CreateNFT.scss';

const CreateNFT = ({
  name,
  setName,
  author,
  setAuthor,
  desc,
  setDesc,
  image,
  setImage,
  onSubmit,
  error,
  setError,
  errorVisibility,
  addTokenFetching,
}) => (
  <div className='CreateNFT'>
    <h3 className='heading'>Create new NFT form</h3>
    <Form className='form' onSubmit={onSubmit}>
      <Form.Group controlId='formName'>
        <Form.Label>Name</Form.Label>
        <Form.Control
          value={name}
          onChange={(ev) => {
            setName(ev.target.value);
            setError({ ...error, name: false });
          }}
          className={error && error.name && 'error'}
          type='text'
          placeholder='Cool token'
        />
      </Form.Group>

      <Form.Group controlId='formAuthor'>
        <Form.Label>Author</Form.Label>
        <Form.Control
          value={author}
          onChange={(ev) => {
            setAuthor(ev.target.value);
            setError({ ...error, author: false });
          }}
          className={error && error.author && 'error'}
          type='text'
          placeholder='Superhero'
        />
      </Form.Group>

      <Form.Group controlId='formDescription'>
        <Form.Label>Description</Form.Label>
        <Form.Control
          value={desc}
          onChange={(ev) => {
            setDesc(ev.target.value);
            setError({ ...error, desc: false });
          }}
          className={error && error.desc && 'error'}
          type='text'
          placeholder='The best token ever'
        />
      </Form.Group>

      <Form.Label>Image</Form.Label>
      <DropzoneComponent
        image={image}
        setImage={setImage}
        error={error}
        setError={setError}
      />

      <Form.Text className={`error-text ${errorVisibility && 'visible'}`}>
        {error && error.errorText}
      </Form.Text>
      <Button variant='secondary' type='submit'>
        Submit
      </Button>
    </Form>
    {addTokenFetching && (
      <Loader
        disableBackground={true}
        text={'Please, be patient. Token is creating...'}
      />
    )}
  </div>
);

const isAlphaNum = (string) => {
  const reg = new RegExp('^[a-zA-Z0-9_]*$');
  return reg.test(string);
};

const enhancer = compose(
  withState('name', 'setName', ''),
  withState('author', 'setAuthor', ''),
  withState('desc', 'setDesc', ''),
  withState('image', 'setImage', ''),
  withState('error', 'setError', null),
  withProps(({ error }) => ({
    errorVisibility:
      error && (error.name || error.author || error.desc || error.image),
  })),
  connect(
    (state) => ({
      addTokenFetching: getMyTokensFetching(state),
    }),
    (dispatch, props) => ({
      fetchAddToken: (data) => dispatch(fetchAddToken(props.currentUser, data)),
    })
  ),
  withHandlers({
    clearForm: ({ setName, setAuthor, setDesc, setImage }) => (ev) => {
      setName('');
      setAuthor('');
      setDesc('');
      setImage('');
    },
  }),
  withHandlers({
    onSubmit: ({
      name,
      author,
      desc,
      image,
      setError,
      fetchAddToken,
      clearForm,
    }) => (ev) => {
      ev.preventDefault();
      if (!name || !author || !desc || !image) {
        setError({
          name: !name,
          author: !author,
          desc: !desc,
          image: !image,
          errorText: 'Please, be sure to fill all fields in the form',
        });
      } else if (
        !isAlphaNum(name) ||
        !isAlphaNum(author) ||
        !isAlphaNum(desc)
      ) {
        setError({
          name: !isAlphaNum(name),
          author: !isAlphaNum(author),
          desc: !isAlphaNum(desc),
          errorText: 'Text fields can include only alphanumeric values',
        });
      } else {
        fetchAddToken(formatForAPI({ name, author, desc, image }));
        setError(null);
        clearForm();
      }
    },
  })
);

export default enhancer(CreateNFT);
